
#include "DbCommon.h"
#include "mod_sml.h"
#include "apr_thread_cond.h"

/*
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
*/

int
create_thread_lock(thread_lock *tlock, void *rd)
{
  apr_status_t status;
  status = apr_thread_mutex_create((apr_thread_mutex_t **) tlock, APR_THREAD_MUTEX_DEFAULT, 
                                    ((request_rec *) rd)->server->process->pconf);
  if (status == APR_SUCCESS) return 0;
  return 1;
}

/*
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
*/

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
  
/*
void
destroy_proc_lock(proc_lock plock)
{
  apr_proc_mutex_destroy(plock);
  return;
}
*/

void
destroy_thread_lock(thread_lock tlock)
{
  apr_thread_mutex_destroy(tlock);
  return;
}

/*
void proc_lock_child_init(proc_lock *plock, char *plockname, void *pool)
{
  apr_proc_mutex_child_init((apr_proc_mutex_t **) plock, plockname, (apr_pool_t *) pool);
  return;
}
*/

struct cond_var1
{
  apr_thread_cond_t *cvar;
  apr_thread_mutex_t *mutex;
};

int 
create_cond_variable(cond_var *cvar, thread_lock l, void *rd)
{
  apr_status_t status;
  struct cond_var1 *tmp = (struct cond_var1 *) malloc(sizeof(struct cond_var1));
  if (!tmp) return 1;
  tmp->mutex = (apr_thread_mutex_t *) l;
  status = apr_thread_cond_create(&(tmp->cvar), ((request_rec *) rd)->server->process->pconf);
  if (status != APR_SUCCESS)
  {
    free(tmp);
    return 1;
  }
  *cvar = tmp;
  return 0;
}

void
destroy_cond_variable(cond_var cvar)
{
  struct cond_var1 *tmp = (struct cond_var1 *) cvar;
  apr_thread_cond_destroy(tmp->cvar);
  free(tmp);
  return;
}

void
signal_cond(cond_var cvar)
{
  struct cond_var1 *tmp = (struct cond_var1 *) cvar;
  apr_thread_cond_signal(tmp->cvar);
  return;
}

void
broadcast_cond(cond_var cvar)
{
  struct cond_var1 *tmp = (struct cond_var1 *) cvar;
  apr_thread_cond_broadcast(tmp->cvar);
  return;
}

void
wait_cond(cond_var cvar)
{
  struct cond_var1 *tmp = (struct cond_var1 *) cvar;
  apr_thread_cond_wait(tmp->cvar,tmp->mutex);
  return;
}

void
raise_overflow(void)
{
  raise_exn ((int) &exn_OVERFLOW);
}
