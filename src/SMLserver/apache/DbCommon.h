
typedef void * proc_lock;
typedef void * thread_lock;

int create_proc_lock(proc_lock *plock, char *plockname, void *rd);
void destroy_proc_lock(proc_lock plock);

int create_thread_lock(thread_lock *tlock, void *rd);
void destroy_thread_lock(thread_lock tlock);

void lock_proc(proc_lock plock);
void unlock_proc(proc_lock plock);

void lock_thread(thread_lock tlock);
void unlock_thread(thread_lock tlock);

void * getSharedMem(void *rd, int size);
void proc_lock_child_init(proc_lock *plock, char *plockname, void *pool);

void raise_overflow(void);

void dblog1(void *rd, char *txt);
void dblog2(void *rd, char *txt, int num);

void * getDbData(int num, void *rd);


int putDbData(int num, void *dbdata, void *rd);

void * getDbData(int num, void *rd);

void removeDbData(int num, void *rd);

void * apsmlGetDBData (int i, void *rd);
int apsmlPutDBData (int i, void *data, void child_init(void *, int, void *, void *),
                           void tmp_shutdown(void *, void *), 
                           void req_cleanup(void *, void *),
                           void *rd);
