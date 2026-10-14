/* Pthread wall sampling on macOS ARM64 and Linux X64. A coordinator targets every registered
 * ML pthread with SIGUSR2. The handler performs only a bounded stack-range scan,
 * clock/errno access and lock-free atomics. It never uses TLS or pthread APIs.
 * Slots are immutable and retained for the session; exited buffers are freed.
 * Gate bits: 1 permits recording, 2 is an in-flight handler. Clearing bit 1 and
 * waiting for bit 2 makes reuse/free safe without stopping the ML thread. */
_Static_assert(ATOMIC_INT_LOCK_FREE == 2 && ATOMIC_LONG_LOCK_FREE == 2 && ATOMIC_LLONG_LOCK_FREE == 2,
               "parallel recorder needs always-lock-free integer atomics");
#define TP_THREAD_LIMIT 1024
#define TP_PAR_SIGNAL SIGUSR2
typedef struct ParallelTimeRecord { _Atomic uintptr_t pc; _Atomic uint64_t tick; } ParallelTimeRecord;
_Static_assert(sizeof(ParallelTimeRecord) == 16, "parallel record must be 16 bytes");
typedef struct ParallelTimeThread {
  pthread_t thread;
  uintptr_t low, high;
  uint64_t id, written, cpu_start;
  ParallelTimeRecord *records;
  _Atomic unsigned live, gate, count;
  _Atomic uint64_t dropped;
} ParallelTimeThread;
static ParallelTimeThread tp_threads[TP_THREAD_LIMIT];
static _Atomic unsigned tp_thread_count, tp_active;
static _Atomic uint64_t tp_routing_dropped;
static uint64_t tp_origin, tp_interval_ticks, tp_written;
static TpTimeScale tp_scale;
static pthread_mutex_t tp_lock = PTHREAD_MUTEX_INITIALIZER;
#ifdef __linux__
static pthread_cond_t tp_changed;
#else
static pthread_cond_t tp_changed = PTHREAD_COND_INITIALIZER;
#endif
static pthread_t tp_coordinator;
static int tp_stopping, tp_closed;
static uint64_t tp_ns(uint64_t tick) {
  uint64_t n = tick-tp_origin;
  return n/tp_scale.denom*tp_scale.numer+n%tp_scale.denom*tp_scale.numer/tp_scale.denom;
}
static uint64_t tp_timestamp(void) { return tp_ns(tp_clock_tick()); }
static void tp_parallel_signal(int sig, siginfo_t *info, void *context) {
  int saved_errno = errno;
  (void)sig; (void)info;
  ucontext_t *uc = context;
  uintptr_t sp = tp_context_sp(uc);
  unsigned limit = atomic_load_explicit(&tp_thread_count,memory_order_acquire);
  for (unsigned i = 0; i < limit; i++) {
    ParallelTimeThread *p = &tp_threads[i];
    if (!atomic_load_explicit(&p->live,memory_order_acquire) || sp < p->low || sp >= p->high) continue;
    unsigned expected = 1;
    if (atomic_compare_exchange_strong_explicit(&p->gate,&expected,3,memory_order_acquire,memory_order_relaxed)) {
      unsigned n = atomic_load_explicit(&p->count,memory_order_relaxed);
      if (n < mlkit_tp_capacity) {
        atomic_store_explicit(&p->records[n].pc,tp_context_pc(uc),memory_order_relaxed);
        atomic_store_explicit(&p->records[n].tick,tp_clock_tick(),memory_order_relaxed);
        atomic_store_explicit(&p->count,n+1,memory_order_release);
      } else {
        uint64_t drops = atomic_load_explicit(&p->dropped,memory_order_relaxed);
        if (drops != UINT64_MAX) atomic_store_explicit(&p->dropped,drops+1,memory_order_relaxed);
      }
      atomic_fetch_and_explicit(&p->gate,~2u,memory_order_release);
    }
    errno = saved_errno;
    return;
  }
  if (atomic_load_explicit(&tp_active,memory_order_relaxed)) atomic_fetch_add_explicit(&tp_routing_dropped,1,memory_order_relaxed);
  errno = saved_errno;
}
static void tp_disable(ParallelTimeThread *p) {
  atomic_fetch_and_explicit(&p->gate,~1u,memory_order_acq_rel);
  while (atomic_load_explicit(&p->gate,memory_order_acquire) & 2) sched_yield();
}
static uint64_t tp_thread_cpu(ParallelTimeThread *p) {
  /* Normal exit runs on the owner. Unjoined workers still alive at process
   * shutdown have no owner-clock observation and are explicitly unavailable. */
  struct timespec cpu;
  if (!pthread_equal(p->thread,pthread_self()) || clock_gettime(CLOCK_THREAD_CPUTIME_ID,&cpu)) return UINT64_MAX;
  return (uint64_t)cpu.tv_sec*UINT64_C(1000000000)+(uint64_t)cpu.tv_nsec;
}
static void tp_thread_event(ParallelTimeThread *p, const char *event) {
  uint64_t cpu = 0;
  if (!strcmp(event,"end")) {
    uint64_t now = tp_thread_cpu(p);
    cpu = now == UINT64_MAX || p->cpu_start == UINT64_MAX || now < p->cpu_start ? UINT64_MAX : now-p->cpu_start;
  }
  emit_record(26,NUMS(p->id,p->id,tp_timestamp(),p->written,atomic_load(&p->dropped),cpu),STRS(event));
}
static void tp_status(int final) {
  uint64_t drops = 0;
  unsigned limit = atomic_load(&tp_thread_count);
  for (unsigned i = 0; i < limit; i++) {
    uint64_t n = atomic_load(&tp_threads[i].dropped);
    drops = UINT64_MAX-drops < n ? UINT64_MAX : drops+n;
  }
  emit_record(25,NUMS(tp_timestamp(),tp_written,drops,atomic_load(&tp_routing_dropped),atomic_load(&tp_active),final),STRS("unobservable"));
}
static void tp_drain_thread(ParallelTimeThread *p) {
  tp_disable(p);
  unsigned count = atomic_load_explicit(&p->count,memory_order_acquire);
  for (unsigned i = 0; i < count; i++) {
    ParallelTimeRecord *r = &p->records[i];
    emit_record(24,NUMS(++tp_written,tp_ns(atomic_load(&r->tick)),atomic_load(&r->pc),p->id,p->id,0,0),NO_STRINGS);
  }
  p->written += count;
  atomic_store(&p->count,0);
}
static void tp_drain_locked(void) {
  unsigned limit = atomic_load(&tp_thread_count);
  /* Disable all streams before serializing: output is an observation gap. */
  for (unsigned i = 0; i < limit; i++) if (atomic_load(&tp_threads[i].live)) tp_disable(&tp_threads[i]);
  for (unsigned i = 0; i < limit; i++) if (atomic_load(&tp_threads[i].live)) tp_drain_thread(&tp_threads[i]);
  for (unsigned i = 0; i < limit; i++) if (atomic_load(&tp_threads[i].live)) atomic_store(&tp_threads[i].gate,atomic_load(&tp_active)?1:0);
}
static void *tp_coordinate(void *unused) {
  (void)unused;
  sigset_t mask; sigemptyset(&mask); sigaddset(&mask,TP_PAR_SIGNAL);
  pthread_sigmask(SIG_BLOCK,&mask,NULL);
  uint64_t due = tp_clock_tick()+tp_interval_ticks;
  unsigned first = 0;
  pthread_mutex_lock(&tp_lock);
  while (!tp_stopping) {
    if (!atomic_load(&tp_active)) {
      pthread_cond_wait(&tp_changed,&tp_lock);
      due = tp_clock_tick()+tp_interval_ticks;
      continue;
    }
    uint64_t now = tp_clock_tick();
    if (now >= due) {
      unsigned limit = atomic_load(&tp_thread_count);
      for (unsigned n = 0; n < limit; n++) {
        ParallelTimeThread *p = &tp_threads[(first+n)%limit];
        if (atomic_load(&p->live) && (atomic_load(&p->gate)&1)) {
          int rc = pthread_kill(p->thread,TP_PAR_SIGNAL);
          if (rc) fail("cannot target registered sampling thread");
        }
      }
      if (limit) first = (first+1)%limit;
      due += ((now-due)/tp_interval_ticks+1)*tp_interval_ticks;
      now = tp_clock_tick();
    }
    uint64_t ticks = due > now ? due-now : 1;
    uint64_t ns = ticks/tp_scale.denom*tp_scale.numer+ticks%tp_scale.denom*tp_scale.numer/tp_scale.denom;
    int rc = tp_cond_wait_ns(&tp_changed,&tp_lock,ns);
    if (rc && rc != ETIMEDOUT) fail("cannot wait for sampling deadline");
  }
  pthread_mutex_unlock(&tp_lock);
  return NULL;
}
static void tp_add_thread(uint64_t id) {
  unsigned n = atomic_load(&tp_thread_count);
  for (unsigned i = 0; i < n; i++) if (tp_threads[i].id == id) {
    if (atomic_load(&tp_threads[i].live) && pthread_equal(tp_threads[i].thread,pthread_self())) return;
    fail("duplicate sampling thread identity");
  }
  if (n == TP_THREAD_LIMIT) fail("time profile supports at most 1024 thread lifetimes per session");
  sigset_t mask;
  pthread_sigmask(SIG_BLOCK,NULL,&mask);
  if (sigismember(&mask,TP_PAR_SIGNAL)) fail("parallel time sampling requires unblocked SIGUSR2 in each ML pthread");
  ParallelTimeThread *p = &tp_threads[n];
  p->thread = pthread_self(); p->id = id;
  if (tp_stack_bounds(p->thread,&p->low,&p->high)) fail("cannot discover sampling thread stack bounds");
  p->records = calloc(mlkit_tp_capacity,sizeof(ParallelTimeRecord));
  if (!p->records) fail("cannot allocate per-thread sampling buffer");
  for (size_t i = 0; i < mlkit_tp_capacity; i++) {
    atomic_init(&p->records[i].pc,0); atomic_init(&p->records[i].tick,0);
  }
  errno = 0; (void)tp_clock_tick();
  p->cpu_start = tp_thread_cpu(p);
  tp_thread_event(p,"start");
  atomic_store(&p->live,1);
  atomic_store_explicit(&tp_thread_count,n+1,memory_order_release);
  atomic_store(&p->gate,atomic_load(&tp_active)?1:0);
  pthread_cond_signal(&tp_changed);
}
void mlkit_tp_thread_enter(int id) {
  if (!mlkit_tp_enabled) return;
  pthread_mutex_lock(&tp_lock);
  if (!tp_closed) tp_add_thread((uint64_t)id);
  pthread_mutex_unlock(&tp_lock);
}
void mlkit_tp_thread_exit(int id) {
  if (!mlkit_tp_enabled) return;
  pthread_mutex_lock(&tp_lock);
  if (!tp_closed) for (unsigned i = 0; i < atomic_load(&tp_thread_count); i++) {
    ParallelTimeThread *p = &tp_threads[i];
    if (p->id != (uint64_t)id || !atomic_load(&p->live)) continue;
    tp_drain_thread(p);
    atomic_store(&p->live,0);
    tp_thread_event(p,"end");
    free(p->records); p->records = NULL;
    tp_status(0);
    break;
  }
  pthread_mutex_unlock(&tp_lock);
}
static void tp_prepare(void) {
  if (mlkit_rp_code_scope != 1) fail("time sampling requires a static executable with function metadata");
  if (mlkit_tp_attribution == MLKIT_TP_DETAILED) fail("parallel time sampling supports passive attribution only");
  if (!mlkit_tp_capacity) mlkit_tp_capacity = 131072;
  if (mlkit_tp_capacity < 2 || mlkit_tp_capacity > 1048576) fail("time buffer capacity must be 2..1048576");
  if (tp_cond_init(&tp_changed)) fail("cannot initialize sampling coordinator clock");
  if (tp_clock_init(&tp_scale)) fail("cannot read timebase");
  tp_origin = tp_clock_tick();
  tp_interval_ticks = (uint64_t)(((__uint128_t)mlkit_tp_interval_us*1000*tp_scale.denom+tp_scale.numer-1)/tp_scale.numer);
  struct sigaction old, action = {0};
  if (sigaction(TP_PAR_SIGNAL,NULL,&old) || old.sa_handler != SIG_DFL) fail("SIGUSR2 already in use by another handler");
  action.sa_sigaction = tp_parallel_signal; action.sa_flags = SA_SIGINFO|SA_RESTART;
  sigemptyset(&action.sa_mask);
  if (sigaction(TP_PAR_SIGNAL,&action,NULL)) fail("cannot install parallel sampling handler");
}
static void tp_begin(void) {
  if (!mlkit_tp_enabled) return;
  atomic_store(&tp_active,!mlkit_tp_initially_paused);
  emit_record(23,NUMS(3,mlkit_tp_interval_us*1000,mlkit_tp_capacity,0,0),STRS("wall","parallel passive 16-byte records; per-thread wall delivery; sum estimates are thread-wall time; missed expirations unobservable",TP_CLOCK_DESCRIPTION));
  tp_add_thread(0);
  tp_status(0);
  if (pthread_create(&tp_coordinator,NULL,tp_coordinate,NULL)) fail("cannot start sampling coordinator");
}
#ifdef PROFILING
static void tp_drain(void) {
  if (!mlkit_tp_enabled) return;
  pthread_mutex_lock(&tp_lock);
  if (!tp_closed) { tp_drain_locked(); tp_status(0); }
  pthread_mutex_unlock(&tp_lock);
}
#endif
static void tp_shutdown(void) {
  if (!mlkit_tp_enabled) return;
  pthread_mutex_lock(&tp_lock);
  if (tp_closed) { pthread_mutex_unlock(&tp_lock); return; }
  tp_closed = 1; tp_stopping = 1; atomic_store(&tp_active,0);
  pthread_cond_signal(&tp_changed);
  pthread_mutex_unlock(&tp_lock);
  pthread_join(tp_coordinator,NULL);
  pthread_mutex_lock(&tp_lock);
  for (unsigned i = 0; i < atomic_load(&tp_thread_count); i++) {
    ParallelTimeThread *p = &tp_threads[i];
    if (!atomic_load(&p->live)) continue;
    tp_drain_thread(p); atomic_store(&p->live,0); tp_thread_event(p,"end");
    free(p->records); p->records = NULL;
  }
  tp_status(1);
  pthread_mutex_unlock(&tp_lock);
}
static uintptr_t tp_change(int running) {
  if (!mlkit_tp_enabled) return 1;
  pthread_mutex_lock(&tp_lock);
  if (!tp_closed) { atomic_store(&tp_active,running); tp_drain_locked(); tp_status(0); pthread_cond_signal(&tp_changed); }
  pthread_mutex_unlock(&tp_lock);
  return 1;
}
uintptr_t mlkit_tp_start(void) { return tp_change(1); }
uintptr_t mlkit_tp_pause(void) { return tp_change(0); }
uintptr_t mlkit_tp_flush(void) {
  if (!mlkit_tp_enabled) return 1;
  pthread_mutex_lock(&tp_lock);
  if (!tp_closed) { tp_drain_locked(); tp_status(0); if (fflush(output)) fail("cannot flush time profile"); }
  pthread_mutex_unlock(&tp_lock);
  return 1;
}
#ifdef PROFILING
static void tp_install_handler(struct sigaction *action) { action->sa_handler = request_sample; }
static void tp_set_region(int running) {
  mlkit_rp_pending = 0;
  next_due_ns = mlkit_rp_interval_us ? timestamp()+mlkit_rp_interval_us*1000 : 0;
  if (!timer_installed) return;
  struct itimerval timer = {0};
  if (running) { timer.it_value.tv_sec=mlkit_rp_interval_us/1000000; timer.it_value.tv_usec=mlkit_rp_interval_us%1000000; timer.it_interval=timer.it_value; }
  if (setitimer(ITIMER_REAL,&timer,NULL)) fail("cannot set region sampling timer");
}
static int tp_region_request(Context ctx) { return mlkit_rp_pending || (active && mlkit_rp_interval_entries && !ctx->rp_entries_left); }
#endif
