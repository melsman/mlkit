/*----------------------------------------------------------------*
 *                         Math                                   *
 *----------------------------------------------------------------*/

#ifndef MATH
#define MATH

/*----------------------------------------------------------------*
 * Include files                                                  *
 * Compiling: cc -Aa -c Math.c                                    *
 *----------------------------------------------------------------*/
#include "Flags.h"
#include "Tagging.h"
#include "String.h"

/*----------------------------------------------------------------*
 *                       Integer operations.                      *
 *----------------------------------------------------------------*/

#if TAG_INTEGERS
#define muliML(x,y,d)  ((d)=1+((x)>>1)*((y)-1))
#define addiML(x,y,d)  ((d)=(x)+(y)-1)
#define subiML(x,y,d)  ((d)=(x)-(y)+1)
#else
#define muliML(x,y,d)  ((d)=(x)*(y))
#define addiML(x,y,d)  ((d)=(x)+(y))
#define subiML(x,y,d)  ((d)=(x)-(y))
#endif

#define  minDefine(A,B) ((A<B)?A:B)

#if TAG_INTEGERS
#define Max_Int 1073741823       /* remember [i] = 2 * i + 1 */
#define Min_Int -1073741824
#define Max_Int_d 1073741823.0
#define Min_Int_d -1073741824.0
#else
#define Max_Int 2147483647
#define Min_Int -2147483647
#define Max_Int_d 2147483647.0
#define Min_Int_d -2147483647.0
#endif

/*----------------------------------------------------------------*
 *        Prototypes for external and internal functions.         *
 *----------------------------------------------------------------*/
int divInt(int x, int y, int exn);
int modInt(int x, int y, int exn);
int quotInt(int x, int y);
int remInt(int x, int y);
int realInt(int d, int x);
int floorFloat(int f);
int ceilFloat(int f);
int roundFloat(int f);
int truncFloat(int f);
int divFloat(int d, int x, int y);

int sqrtFloat(int d, int s);
int sinFloat(int d, int s);
int cosFloat(int d, int s);
int atanFloat(int d, int s);
int asinFloat(int d, int s);
int acosFloat(int d, int s);
int atan2Float(int d, int y, int x);
int expFloat(int d, int s);
int powFloat(int d, int x, int y);
int lnFloat(int d, int s);
int sinhFloat(int d, int s);
int coshFloat(int d, int s);
int tanhFloat(int d, int s);
int isnanFloat(int s);
int posInfFloat(int d);
int negInfFloat(int d);

StringDesc* stringOfFloat(int rAddr, int f);
StringDesc* stringOfFloatProf(int rAddr, int f, int pPlint);
StringDesc* generalStringOfFloat(int rAddr, StringDesc *str, int f);
StringDesc* generalStringOfFloatProf(int rAddr, StringDesc *str, int f, int pPoint);

/* For basislib Math structure */
int sml_sqrt(int d, int s);

void printReal(double *n);

#endif /*MATH*/
