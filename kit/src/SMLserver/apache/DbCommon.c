
#include "DbCommon.h"
#include "mod_sml.h"

void *
getSharedMem(void *rd, int size)
{
 return (void *) apr_shm_baseaddr_get(((request_data *) rd)->ctx->cachelock.shm);
}

int
create_proc_lock(proc_lock *plock, char *plockname, void *rd)
{
  apr_status_t status;
  status = apr_proc_mutex_create((apr_proc_mutex_t **) plock, plockname, APR_LOCK_DEFAULT, 
                                    ((request_rec *) rd)->server->process->pconf);
  if (status == APR_SUCCESS) return 0;
  return 1;
}

int
create_thread_lock(thread_lock *tlock, void *rd)
{
  apr_status_t status;
  status = apr_thread_mutex_create((apr_thread_mutex_t **) tlock, APR_THREAD_MUTEX_DEFAULT, 
                                    ((request_rec *) rd)->server->process->pconf);
  if (status == APR_SUCCESS) return 0;
  return 1;
}

void
lock_proc(proc_lock plock)
{
  apr_proc_mutex_lock(plock);
  return;
}

void
unlock_proc(proc_lock plock)
{
  apr_proc_mutex_unlock(plock);
  return;
}
  
void
lock_thread(thread_lock tlock)
{
  apr_thread_mutex_lock(tlock);
  return;
}

void
unlock_thread(thread_lock tlock)
{
  apr_thread_mutex_unlock(tlock);
  return;
}
  
void
destroy_proc_lock(proc_lock plock)
{
  apr_proc_mutex_destroy(plock);
  return;
}

void
destroy_thread_lock(thread_lock tlock)
{
  apr_thread_mutex_destroy(tlock);
  return;
}

void proc_lock_child_init(proc_lock *plock, char *plockname, void *pool)
{
  apr_proc_mutex_child_init((apr_proc_mutex_t **) plock, plockname, (apr_pool_t *) pool);
  return;
}

void
raise_overflow(void)
{
  raise_exn ((int) &exn_OVERFLOW);
}
