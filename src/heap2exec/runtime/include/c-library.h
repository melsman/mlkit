/* c-library.h
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 */

#ifndef _C_LIBRARY_
#define _C_LIBRARY_

/* a pointer to a library initialization function; it is passed the
 * list of command-line arguments.
 */
typedef void (*clib_init_fn_t) (int, char **);

/* a pointer to an ML callable C function */
typedef ml_val_t (*cfunc_t) (ml_state_t *, ml_val_t);

/* an element in the table of name/function pairs. */
typedef struct {
    const char	    *name;
    cfunc_t	    cfunc;
} cfunc_binding_t;

/* The representation of a library of ML callable C functions */
typedef struct {
    const char	    *libName;	/* the library name */
    const char	    *version;
    const char	    *date;
    clib_init_fn_t  initFn;	/* an optional initialization function */
    cfunc_binding_t *cfuns;	/* the list of C function bindings, which is */
				/* terminated by {0, 0}. */
} c_library_t;


/* A C function prototype declaration */
#define CFUNC_PROTO(NAME, FUNC, MLTYPE)	\
	extern ml_val_t FUNC (ml_state_t *msp, ml_val_t arg);

/* A C function binding */
#define CFUNC_BIND(NAME, FUNC, MLTYPE)	\
    { NAME, FUNC },

/* the terminator for a C function list */
#define CFUNC_NULL_BIND		{ NIL(const char *), NIL(cfunc_t) }

#endif /* !_C_LIBRARY_ */
