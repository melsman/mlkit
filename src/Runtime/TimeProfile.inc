/* Single-thread native wall recorder. Included after the wire encoder.
 * Only tp_signal executes asynchronously. No metadata lookup, allocation,
 * locks, stack walking or I/O occurs there. Explicit drains block delivery
 * while reusing the single buffer. All signal-visible mutable objects
 * are always-lock-free atomics. Buffers remain allocated until process exit. */
__attribute__((weak)) const volatile uintptr_t mlkit_tp_attribution_capable = 0;
extern const volatile uintptr_t mlkit_tp_gc_address;
_Atomic uintptr_t mlkit_tp_context;
uintptr_t mlkit_tp_gc_enter(void) { return atomic_exchange_explicit(&mlkit_tp_context,3,memory_order_relaxed); }
void mlkit_tp_gc_leave(uintptr_t saved) { atomic_store_explicit(&mlkit_tp_context,saved,memory_order_relaxed); }
int mlkit_tp_enabled, mlkit_tp_initially_paused;
int mlkit_tp_attribution = MLKIT_TP_AUTO;
uint64_t mlkit_tp_interval_us = 1000;
/* Zero selects the default 2 MiB budget after choosing the record layout. */
size_t mlkit_tp_capacity;
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
#if defined(MLKIT_TP_NATIVE) && defined(PARALLEL)
#include "TimeProfileParallel.inc"
#else
void mlkit_tp_thread_enter(int id) { (void)id; }
void mlkit_tp_thread_exit(int id) { (void)id; }
#ifdef MLKIT_TP_NATIVE
_Static_assert(ATOMIC_INT_LOCK_FREE == 2 && ATOMIC_LONG_LOCK_FREE == 2 && ATOMIC_LLONG_LOCK_FREE == 2,
               "time recorder requires always-lock-free native integer atomics");
typedef struct TimeRecord {
  _Atomic uintptr_t pc;
  _Atomic uint64_t tick;
} TimeRecord;
typedef struct DetailedTimeRecord {
  TimeRecord sample;
  _Atomic unsigned state;
  _Atomic uintptr_t origin_pc;
} DetailedTimeRecord;
_Static_assert(sizeof(TimeRecord) == 16, "passive time record must be 16 bytes");
_Static_assert(sizeof(DetailedTimeRecord) == 32, "detailed time record must be 32 bytes");
static TimeRecord *tp_buffer_records;
#ifdef ENABLE_GC
extern _Atomic ssize_t doing_gc;
#endif
static DetailedTimeRecord *tp_detailed_records;
/* Selected before installing the handler and immutable during recording. */
static int tp_detailed;
static _Atomic unsigned tp_count, tp_active, tp_recorder;
static _Atomic unsigned tp_region_active, tp_region_pending;
static _Atomic uint64_t tp_due, tp_region_due, tp_dropped, tp_routing_dropped;
static uint64_t tp_origin, tp_interval_ticks, tp_region_ticks, tp_written, tp_alarm_us;
static TpTimeScale tp_scale;
static uintptr_t tp_stack_low, tp_stack_high;
static sigset_t tp_mask;
static int tp_draining, tp_closed;
static uint64_t tp_ns(uint64_t tick) {
  uint64_t n = tick-tp_origin;
  return n/tp_scale.denom*tp_scale.numer + n%tp_scale.denom*tp_scale.numer/tp_scale.denom;
}
static uint64_t tp_timestamp(void) { return tp_ns(tp_clock_tick()); }
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
  uintptr_t sp = tp_context_sp(uc);
  if (sp < tp_stack_low || sp >= tp_stack_high) {
    if (atomic_load_explicit(&tp_active,memory_order_relaxed)) tp_loss(&tp_routing_dropped);
    errno = saved_errno;
    return;
  }
  uint64_t now = tp_clock_tick();
  if (atomic_load_explicit(&tp_region_active,memory_order_relaxed) && tp_region_ticks &&
      now >= atomic_load_explicit(&tp_region_due,memory_order_relaxed)) {
    atomic_store_explicit(&tp_region_pending,1,memory_order_relaxed);
    atomic_store_explicit(&tp_region_due,now+tp_region_ticks,memory_order_relaxed);
  }
  if (atomic_load_explicit(&tp_active,memory_order_relaxed) &&
      now >= atomic_load_explicit(&tp_due,memory_order_relaxed)) {
    uint64_t due = atomic_load_explicit(&tp_due,memory_order_relaxed);
    atomic_store_explicit(&tp_due,due+((now-due)/tp_interval_ticks+1)*tp_interval_ticks,memory_order_relaxed);
    unsigned n = atomic_load_explicit(&tp_count,memory_order_relaxed);
    if (n < mlkit_tp_capacity) {
      TimeRecord *record = tp_detailed ? &tp_detailed_records[n].sample : &tp_buffer_records[n];
#ifdef ENABLE_GC
      unsigned in_gc = atomic_load_explicit(&doing_gc,memory_order_relaxed) != 0;
#else
      unsigned in_gc = 0;
#endif
      /* Passive profiles aggregate the entire GC extent, including helpers. */
      uintptr_t pc = !tp_detailed && in_gc ? mlkit_tp_gc_address : tp_context_pc(uc);
      atomic_store_explicit(&record->pc,pc,memory_order_relaxed);
      atomic_store_explicit(&record->tick,now,memory_order_relaxed);
      if (tp_detailed) {
        DetailedTimeRecord *detail = &tp_detailed_records[n];
        uintptr_t attribution = atomic_load_explicit(&mlkit_tp_context,memory_order_relaxed);
        unsigned state = atomic_load_explicit(&tp_recorder,memory_order_relaxed) ? 1 : in_gc ? 3 : attribution & 3;
        atomic_store_explicit(&detail->state,state,memory_order_relaxed);
        atomic_store_explicit(&detail->origin_pc,state == 2 ? attribution & ~(uintptr_t)3 : 0,memory_order_relaxed);
      }
      atomic_store_explicit(&tp_count,n+1,memory_order_release);
    } else tp_loss(&tp_dropped);
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
  if (mlkit_rp_code_scope != 1) fail("time sampling requires a static executable with function metadata");
  if (mlkit_tp_attribution == MLKIT_TP_DETAILED && mlkit_tp_attribution_capable != 2)
    fail("detailed C attribution requires single-thread code compiled with -rp; use passive attribution for ordinary builds");
  tp_detailed = mlkit_tp_attribution != MLKIT_TP_PASSIVE && mlkit_tp_attribution_capable == 2;
  if (!mlkit_tp_capacity) mlkit_tp_capacity = tp_detailed ? 65536 : 131072;
  if (mlkit_tp_capacity < 2 || mlkit_tp_capacity > 1048576) fail("time buffer capacity must be 2..1048576");
  sigemptyset(&tp_mask); sigaddset(&tp_mask,SIGALRM);
  sigset_t current;
  if (sigprocmask(SIG_BLOCK,NULL,&current)) fail("cannot inspect signal mask");
  if (sigismember(&current,SIGALRM)) fail("time sampling requires an unblocked SIGALRM");
  if (tp_clock_init(&tp_scale) || !tp_scale.numer || !tp_scale.denom) fail("cannot read timebase");
  errno = 0; /* Resolve errno/timestamp stubs before installing the handler. */
  tp_origin = tp_clock_tick();
  tp_interval_ticks = tp_ticks(mlkit_tp_interval_us);
  tp_region_ticks = tp_ticks(mlkit_rp_interval_us);
  if (tp_stack_bounds(pthread_self(),&tp_stack_low,&tp_stack_high)) fail("cannot discover sampling stack bounds");
  {
    if (tp_detailed) tp_detailed_records = calloc(mlkit_tp_capacity,sizeof(DetailedTimeRecord));
    else tp_buffer_records = calloc(mlkit_tp_capacity,sizeof(TimeRecord));
    if (!(tp_detailed ? (void *)tp_detailed_records : (void *)tp_buffer_records))
      fail("cannot allocate time sample buffer");
    /* Touch every page and initialize atomics outside signal context. */
    for (size_t i = 0; i < mlkit_tp_capacity; i++) {
      TimeRecord *record = tp_detailed ? &tp_detailed_records[i].sample : &tp_buffer_records[i];
      atomic_init(&record->pc,0);
      atomic_init(&record->tick,0);
      if (tp_detailed) {
        atomic_init(&tp_detailed_records[i].state,0);
        atomic_init(&tp_detailed_records[i].origin_pc,0);
      }
    }
  }
}
static void tp_begin(void) {
  if (!mlkit_tp_enabled) return;
  atomic_store(&tp_active,!mlkit_tp_initially_paused);
  atomic_store(&tp_due,tp_clock_tick()+tp_interval_ticks);
  emit_record(23,NUMS(2,mlkit_tp_interval_us*1000,mlkit_tp_capacity,0,0),
              STRS("wall",tp_detailed ?
                   "detailed 32-byte buffer records; elapsed delivery samples; blocked locations observed, missed expirations not reconstructed" :
                   "passive 16-byte buffer records; elapsed delivery samples; blocked locations observed, missed expirations not reconstructed",
                   TP_CLOCK_DESCRIPTION));
  tp_status(0);
}
static void tp_install_handler(struct sigaction *action) {
  action->sa_sigaction = tp_signal;
  action->sa_flags = SA_SIGINFO | SA_RESTART;
}
#ifdef PROFILING
static void tp_set_region(int running) {
  if (!mlkit_tp_enabled) return;
  sigset_t previous;
  tp_block(&previous);
  mlkit_rp_pending = 0;
  atomic_store(&tp_region_pending,0);
  atomic_store(&tp_region_active,running);
  next_due_ns = mlkit_rp_interval_us ? timestamp()+mlkit_rp_interval_us*1000 : 0;
  atomic_store(&tp_region_due,tp_clock_tick()+tp_region_ticks);
  tp_alarm();
  tp_unblock(&previous);
}
#endif
static void tp_drain(void) {
  if (!mlkit_tp_enabled || !output || tp_draining) return;
  tp_draining = 1;
  sigset_t previous;
  tp_block(&previous);
  atomic_store(&tp_recorder,1);
  unsigned count = atomic_load_explicit(&tp_count,memory_order_acquire);
  for (unsigned i = 0; i < count; i++) {
    TimeRecord *r = tp_detailed ? &tp_detailed_records[i].sample : &tp_buffer_records[i];
    unsigned state = tp_detailed ? atomic_load(&tp_detailed_records[i].state) : 0;
    uintptr_t origin = tp_detailed ? atomic_load(&tp_detailed_records[i].origin_pc) : 0;
    emit_record(24,NUMS(++tp_written,tp_ns(atomic_load(&r->tick)),atomic_load(&r->pc),
                        0,0,state,origin),NO_STRINGS);
  }
  atomic_store(&tp_count,0);
  atomic_store(&tp_recorder,0);
  tp_unblock(&previous);
  if (count) tp_status(0);
  tp_draining = 0;
}
#ifdef PROFILING
static int tp_region_request(Context ctx) {
  sigset_t previous;
  tp_block(&previous);
  int requested = atomic_load(&tp_region_pending) ||
    (active && mlkit_rp_interval_entries && !ctx->rp_entries_left);
  if (!requested) mlkit_rp_pending = 0;
  tp_unblock(&previous);
  return requested;
}
#endif
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
  atomic_store(&tp_due,tp_clock_tick()+tp_interval_ticks);
  tp_alarm();
  tp_unblock(&previous);
  tp_drain();
  tp_status(0);
  return 1;
}
uintptr_t mlkit_tp_start(void) { return tp_change(1); }
uintptr_t mlkit_tp_pause(void) { return tp_change(0); }
uintptr_t mlkit_tp_flush(void) {
  if (!mlkit_tp_enabled || !output || tp_closed) return 1;
  tp_drain();
  if (output && fflush(output)) fail("cannot flush time profile");
  return 1;
}
#else
static void tp_prepare(void) { fail("time sampling supports static macOS ARM64 or Linux X64 native threads; Argobots and other platforms are unsupported"); }
static void tp_begin(void) {}
static void tp_install_handler(struct sigaction *action) { (void)action; }
#ifdef PROFILING
static void tp_set_region(int running) { (void)running; }
static int tp_region_request(Context ctx) { (void)ctx; return 0; }
#endif
static void tp_drain(void) {}
static void tp_shutdown(void) {}
uintptr_t mlkit_tp_start(void) { return 1; }
uintptr_t mlkit_tp_pause(void) { return 1; }
uintptr_t mlkit_tp_flush(void) { tp_drain(); return 1; }
#endif

#endif /* parallel / single-thread implementation */
