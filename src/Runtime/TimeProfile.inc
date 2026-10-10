/* T3: single-thread macOS ARM64 recorder. Included after the wire encoder.
 * Only tp_signal executes asynchronously. No metadata lookup, allocation,
 * locks, stack walking or I/O occurs there. All signal-visible mutable objects
 * are always-lock-free atomics. Buffers remain allocated until process exit. */
__attribute__((weak)) const volatile uintptr_t mlkit_tp_attribution_capable = 0;
_Atomic uintptr_t mlkit_tp_context;
uintptr_t mlkit_tp_gc_enter(void) { return atomic_exchange_explicit(&mlkit_tp_context,3,memory_order_relaxed); }
void mlkit_tp_gc_leave(uintptr_t saved) { atomic_store_explicit(&mlkit_tp_context,saved,memory_order_relaxed); }
int mlkit_tp_enabled, mlkit_tp_initially_paused;
uint64_t mlkit_tp_interval_us = 1000;
size_t mlkit_tp_capacity = 4096;
int mlkit_tp_parse_interval(const char *s) {
  if (*s < '0' || *s > '9') return 0;
  errno = 0;
  char *end;
  unsigned long long n = strtoull(s,&end,10);
  uint64_t scale = !strcmp(end,"us") ? 1 : !strcmp(end,"ms") ? 1000 : !strcmp(end,"s") ? 1000000 : 0;
  if (errno || !n || !scale || n > UINT64_C(1000000)/scale) return 0;
  mlkit_tp_interval_us = n*scale;
  return 1;
}
#ifdef MLKIT_TP_NATIVE
_Static_assert(ATOMIC_INT_LOCK_FREE == 2 && ATOMIC_LONG_LOCK_FREE == 2 && ATOMIC_LLONG_LOCK_FREE == 2,
               "time recorder requires always-lock-free native integer atomics");
typedef struct TimeRecord {
  _Atomic uintptr_t pc;
  _Atomic uint64_t tick;
  _Atomic unsigned state;
  _Atomic uintptr_t origin_pc;
} TimeRecord;
static TimeRecord *tp_buffers[2];
static _Atomic unsigned tp_count[2], tp_buffer, tp_active, tp_recorder;
static _Atomic unsigned tp_region_active, tp_region_pending;
static _Atomic uint64_t tp_due, tp_region_due, tp_dropped, tp_routing_dropped;
static uint64_t tp_origin, tp_interval_ticks, tp_region_ticks, tp_written, tp_alarm_us;
static mach_timebase_info_data_t tp_scale;
static uintptr_t tp_stack_low, tp_stack_high;
static sigset_t tp_mask;
static int tp_draining, tp_closed;
static uint64_t tp_ns(uint64_t tick) {
  uint64_t n = tick-tp_origin;
  return n/tp_scale.denom*tp_scale.numer + n%tp_scale.denom*tp_scale.numer/tp_scale.denom;
}
static uint64_t tp_timestamp(void) { return tp_ns(mach_absolute_time()); }
static uint64_t tp_ticks(uint64_t us) {
  return (uint64_t)(((__uint128_t)us*1000*tp_scale.denom+tp_scale.numer-1)/tp_scale.numer);
}
static void tp_block(sigset_t *previous) {
  if (sigprocmask(SIG_BLOCK,&tp_mask,previous)) fail("cannot block time sampling signal");
}
static void tp_unblock(const sigset_t *previous) {
  if (sigprocmask(SIG_SETMASK,previous,NULL)) fail("cannot restore time sampling signal mask");
}
static void tp_loss(_Atomic uint64_t *counter) {
  uint64_t n = atomic_load_explicit(counter,memory_order_relaxed);
  while (n != UINT64_MAX && !atomic_compare_exchange_weak_explicit(
    counter,&n,n+1,memory_order_relaxed,memory_order_relaxed)) {}
}
static void tp_signal(int sig, siginfo_t *info, void *context) {
  int saved_errno = errno;
  (void)sig; (void)info;
  ucontext_t *uc = context;
  uintptr_t sp = (uintptr_t)__darwin_arm_thread_state64_get_sp(uc->uc_mcontext->__ss);
  if (sp < tp_stack_low || sp >= tp_stack_high) {
    if (atomic_load_explicit(&tp_active,memory_order_relaxed)) tp_loss(&tp_routing_dropped);
    errno = saved_errno;
    return;
  }
  uint64_t now = mach_absolute_time();
  if (atomic_load_explicit(&tp_region_active,memory_order_relaxed) && tp_region_ticks &&
      now >= atomic_load_explicit(&tp_region_due,memory_order_relaxed)) {
    atomic_store_explicit(&tp_region_pending,1,memory_order_relaxed);
    atomic_store_explicit(&tp_region_due,now+tp_region_ticks,memory_order_relaxed);
  }
  if (atomic_load_explicit(&tp_active,memory_order_relaxed) &&
      now >= atomic_load_explicit(&tp_due,memory_order_relaxed)) {
    uint64_t due = atomic_load_explicit(&tp_due,memory_order_relaxed);
    atomic_store_explicit(&tp_due,due+((now-due)/tp_interval_ticks+1)*tp_interval_ticks,memory_order_relaxed);
    unsigned b = atomic_load_explicit(&tp_buffer,memory_order_relaxed);
    unsigned n = atomic_load_explicit(&tp_count[b],memory_order_relaxed);
    if (n < mlkit_tp_capacity) {
      TimeRecord *record = &tp_buffers[b][n];
      atomic_store_explicit(&record->pc,
        (uintptr_t)__darwin_arm_thread_state64_get_pc(uc->uc_mcontext->__ss),memory_order_relaxed);
      atomic_store_explicit(&record->tick,now,memory_order_relaxed);
      uintptr_t attribution = atomic_load_explicit(&mlkit_tp_context,memory_order_relaxed);
      unsigned state = atomic_load_explicit(&tp_recorder,memory_order_relaxed) ? 1 : attribution & 3;
      atomic_store_explicit(&record->state,state,memory_order_relaxed);
      atomic_store_explicit(&record->origin_pc,state == 2 ? attribution & ~(uintptr_t)3 : 0,memory_order_relaxed);
      atomic_store_explicit(&tp_count[b],n+1,memory_order_release);
    } else tp_loss(&tp_dropped);
    if (n+1 >= mlkit_tp_capacity/2) atomic_store_explicit(&mlkit_rp_pending,1,memory_order_relaxed);
  }
  if (atomic_load_explicit(&tp_region_pending,memory_order_relaxed))
    atomic_store_explicit(&mlkit_rp_pending,1,memory_order_relaxed);
  errno = saved_errno;
}
static void tp_status(int final) {
  emit_record(25,NUMS(tp_timestamp(),tp_written,atomic_load(&tp_dropped),
                      atomic_load(&tp_routing_dropped),atomic_load(&tp_active),final),STRS("unobservable"));
}
static void tp_alarm(void) {
  if (!timer_installed) return;
  uint64_t interval = atomic_load(&tp_active) ? mlkit_tp_interval_us : 0;
  if (atomic_load(&tp_region_active) && mlkit_rp_interval_us &&
      (!interval || mlkit_rp_interval_us < interval)) interval = mlkit_rp_interval_us;
  if (interval == tp_alarm_us) return; /* Region output must not reset the time timer. */
  struct itimerval timer = {0};
  timer.it_value.tv_sec = interval/1000000;
  timer.it_value.tv_usec = interval%1000000;
  timer.it_interval = timer.it_value;
  if (setitimer(ITIMER_REAL,&timer,NULL)) fail("cannot set shared profiling timer");
  tp_alarm_us = interval;
}
static void tp_prepare(void) {
  if (mlkit_tp_attribution_capable != 2) fail("recompile time profiling code with the T4 compiler");
  if (mlkit_rp_code_scope != 1) fail("time sampling requires a static executable with T2 metadata");
  for (size_t i = 0; mlkit_rp_code_units[i]; i++)
    if (mlkit_rp_code_units[i]->version != 2) fail("rebuild all ML libraries for time attribution");
  if (mlkit_tp_capacity < 2 || mlkit_tp_capacity > 1048576) fail("time buffer capacity must be 2..1048576");
  sigemptyset(&tp_mask); sigaddset(&tp_mask,SIGALRM);
  sigset_t current;
  if (sigprocmask(SIG_BLOCK,NULL,&current)) fail("cannot inspect signal mask");
  if (sigismember(&current,SIGALRM)) fail("time sampling requires an unblocked SIGALRM");
  if (mach_timebase_info(&tp_scale) || !tp_scale.numer || !tp_scale.denom) fail("cannot read timebase");
  errno = 0; /* Resolve errno/timestamp stubs before installing the handler. */
  tp_origin = mach_absolute_time();
  tp_interval_ticks = tp_ticks(mlkit_tp_interval_us);
  tp_region_ticks = tp_ticks(mlkit_rp_interval_us);
  tp_stack_high = (uintptr_t)pthread_get_stackaddr_np(pthread_self());
  tp_stack_low = tp_stack_high-pthread_get_stacksize_np(pthread_self());
  for (unsigned b = 0; b < 2; b++) {
    tp_buffers[b] = calloc(mlkit_tp_capacity,sizeof(TimeRecord));
    if (!tp_buffers[b]) fail("cannot allocate time sample buffer");
    /* Touch every page and initialize atomics outside signal context. */
    for (size_t i = 0; i < mlkit_tp_capacity; i++) {
      atomic_init(&tp_buffers[b][i].pc,0);
      atomic_init(&tp_buffers[b][i].tick,0);
      atomic_init(&tp_buffers[b][i].state,0);
      atomic_init(&tp_buffers[b][i].origin_pc,0);
    }
  }
}
static void tp_begin(void) {
  if (!mlkit_tp_enabled) return;
  atomic_store(&tp_active,!mlkit_tp_initially_paused);
  atomic_store(&tp_due,mach_absolute_time()+tp_interval_ticks);
  emit_record(23,NUMS(2,mlkit_tp_interval_us*1000,mlkit_tp_capacity,0,0),
              STRS("wall","elapsed delivery samples; blocked locations observed, missed expirations not reconstructed",
                   "mach-uptime-ns (excludes machine sleep)"));
  tp_status(0);
}
static void tp_install_handler(struct sigaction *action) {
  action->sa_sigaction = tp_signal;
  action->sa_flags = SA_SIGINFO | SA_RESTART;
}
static void tp_set_region(int running) {
  if (!mlkit_tp_enabled) return;
  sigset_t previous;
  tp_block(&previous);
  mlkit_rp_pending = 0;
  atomic_store(&tp_region_pending,0);
  atomic_store(&tp_region_active,running);
  next_due_ns = mlkit_rp_interval_us ? timestamp()+mlkit_rp_interval_us*1000 : 0;
  atomic_store(&tp_region_due,mach_absolute_time()+tp_region_ticks);
  tp_alarm();
  tp_unblock(&previous);
}
static void tp_drain(void) {
  if (!mlkit_tp_enabled || !output || tp_draining) return;
  tp_draining = 1;
  atomic_store(&tp_recorder,1);
  sigset_t previous;
  tp_block(&previous);
  unsigned b = atomic_load(&tp_buffer);
  unsigned count = atomic_load_explicit(&tp_count[b],memory_order_acquire);
  atomic_store(&tp_count[b^1],0);
  atomic_store(&tp_buffer,b^1);
  tp_unblock(&previous);
  for (unsigned i = 0; i < count; i++) {
    TimeRecord *r = &tp_buffers[b][i];
    emit_record(24,NUMS(++tp_written,tp_ns(atomic_load(&r->tick)),atomic_load(&r->pc),
                        0,0,atomic_load(&r->state),atomic_load(&r->origin_pc)),NO_STRINGS);
  }
  if (count) tp_status(0);
  atomic_store(&tp_recorder,0);
  tp_draining = 0;
}
static int tp_region_request(Context ctx) {
  sigset_t previous;
  tp_block(&previous);
  int requested = atomic_load(&tp_region_pending) ||
    (active && mlkit_rp_interval_entries && !ctx->rp_entries_left);
  if (!requested) mlkit_rp_pending = 0;
  tp_unblock(&previous);
  return requested;
}
static void tp_shutdown(void) {
  if (!mlkit_tp_enabled || tp_closed) return;
  sigset_t previous;
  tp_block(&previous);
  atomic_store(&tp_active,0);
  tp_alarm();
  tp_unblock(&previous);
  tp_drain();
  tp_status(1);
  tp_closed = 1;
}
static uintptr_t tp_change(int running) {
  if (!mlkit_tp_enabled || !output || tp_closed) return 1;
  sigset_t previous;
  tp_block(&previous);
  atomic_store(&tp_active,running);
  atomic_store(&tp_due,mach_absolute_time()+tp_interval_ticks);
  tp_alarm();
  tp_unblock(&previous);
  tp_drain();
  tp_status(0);
  return 1;
}
uintptr_t mlkit_tp_start(void) { return tp_change(1); }
uintptr_t mlkit_tp_pause(void) { return tp_change(0); }
uintptr_t mlkit_tp_flush(void) {
  tp_drain();
  if (output && fflush(output)) fail("cannot flush time profile");
  return 1;
}
#else
static void tp_prepare(void) { fail("time sampling supports only single-thread macOS ARM64"); }
static void tp_begin(void) {}
static void tp_install_handler(struct sigaction *action) { (void)action; }
static void tp_set_region(int running) { (void)running; }
static void tp_drain(void) {}
static int tp_region_request(Context ctx) { (void)ctx; return 0; }
static void tp_shutdown(void) {}
uintptr_t mlkit_tp_start(void) { return 1; }
uintptr_t mlkit_tp_pause(void) { return 1; }
uintptr_t mlkit_tp_flush(void) { return 1; }
#endif
