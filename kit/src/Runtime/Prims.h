/* Interface with C primitives.       */
/* Used by the KAM                    */
/* Inspired by the Moscow ML compiler */

#ifndef _PRIMS_
#define _PRIMS_

typedef int (*c_primitive)();
extern c_primitive cprim[];

#endif /* _PRIMS_ */
