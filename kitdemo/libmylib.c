#include <sys/types.h>
#include <dirent.h>
#include <stdio.h>
#include "../src/Runtime/List.h"
#include "../src/Runtime/String.h"
#include "../src/Runtime/Exception.h"
#include "../src/Runtime/Region.h"
#include "../src/Runtime/Tagging.h"

/* This file is compiled with gcc:
 *   gcc -c my_lib.c
 * or
 *   gcc -c -DPROFILING my_lib.c
 * when profiling is enabled. */

/* In this version of power we have inserted explicit conversion of
 * integers even though it is not necessary (the converter functions
 * are the identity map). For portability, it is a good idea to use
 * the converter functions. The function also works when profiling is
 * enabled. */

long
power(long base, long n) {
  long p;
  base = convertIntToC(base);
  n = convertIntToC(n);
  for (p = 1; n > 0; --n) p = p * base;
  p = convertIntToML(p);
  return p;
}

/* This version of the power function assumes that auto convertion
 * is used. */
long
power_auto(long base, long n) {
  long p;
  for (p = 1; n > 0; --n) p = p * base;
  return p;
}

/* A variant of the power function where the base is a float. The base
 * and res are boxed, i.e. they are pointers to pre-allocated
 * space. We retrieve the real from base and put the result in memory
 * pointed at by res. The function also works when profiling is
 * enabled. */
ssize_t
power_real(ssize_t res, ssize_t base, long n) {
  double p, base_real;
  base_real = get_d(base);
  n = convertIntToC(n);
  for (p = 1; n > 0; --n) p = p * base_real;
  get_d(res) = p;
  set_dtag(res);
  return res;
}

/* A variant of power where an exception may be raised. We raise exn
 * when base is negative. The function also works when profiling is
 * enabled. */
ssize_t
power_exn(ssize_t res, Context ctx, ssize_t base, long n, uintptr_t exn) {
  double p, base_real;
  base_real = get_d(base);
  if (base_real < 0.0) {
    printf("Now raising exception.\n");
    raise_exn(ctx, exn);
    return 0; /* This return statement is necessary
	       * with the C backend */
  }
  n = convertIntToC(n);
  for (p = 1; n > 0; --n) p = p * base_real;
  get_d(res) = p;
  set_dtag(res);
  return res;
}

/* This example shows how to traverse a list.  We run through the list
 * while the next element in the list is a CONS. Example of the string
 * list representation: ["ABC","DEF","GHI","JKL"] =
 * CONS("ABC",CONS("DEF",CONS("GHI",CONS("JKL",NIL)))). Works also
 * when profiling is enabled. */
long
print_string_list(uintptr_t strs) {
  uintptr_t ys;
  for (ys=strs; isCONS(ys); ys=tl(ys))
    printStringML((StringDesc *) hd(ys));
  return 0;
}


/* The function real_list returns a list of reals.  The list is
 * constructed forwards. The function allocates memory in infinite
 * regions, thus, we have also made a version for profiling.  The
 * algorithm for constructing a list forwards are as follows: (The
 * treatment of the first element is special because we have to return
 * a pointer to it.)
 *        ml_elem = ...
 *        allocRecordML(pairRho, 2, pair);
 *        first(pair) = (long) ml_elem;
 *        makeCONS(pair, cons);
 *        res = (long) cons;
 *
 *        while (more elements) {
 *          ml_elem = ...
 *          allocRecordML(pairRho, 2, temp_pair);
 *          first(temp_pair) = (long) ml_elem;
 *          makeCONS(temp_pair, cons);
 *          second(pair) = (long) cons;
 *          pair = temp_pair;
 *        }
 *        makeNIL(cons);
 *        second(pair) = (long)cons;
 *        return res;
 * The algorithm is explained in the manual ``Programming With Regions
 * in the MLKit'' found in the doc directory. Note, that we also try
 * to reset the two infinite regions. */

#ifndef PROFILING
uintptr_t
real_list(Region pairRho, Region realRho) {
  double elem;
  uintptr_t *realPtr, *pair, *consPtr, *temp_pair, i, res;

  if (is_inf_and_atbot(pairRho))
    resetRegion(pairRho);
  if (is_inf_and_atbot(realRho))
    resetRegion(realRho);

  /* First element is special because we have to return
   * a pointer to it. */
  elem = 2.5;
  allocReal(realRho, realPtr);
  convertRealToML(elem, realPtr);
  allocRecordML(pairRho, 2, pair);
  first(pair) = (uintptr_t) realPtr;
  makeCONS(pair, consPtr);
  res = (uintptr_t) consPtr;
  for(i=1;i<10;i++) {
    elem = elem + 2.5;
    allocReal(realRho, realPtr);
    convertRealToML(elem, realPtr);
    allocRecordML(pairRho, 2, temp_pair);
    first(temp_pair) = (uintptr_t) realPtr;
    makeCONS(temp_pair, consPtr);
    second(pair) = (uintptr_t) consPtr;
    pair = temp_pair;
  }
  makeNIL(consPtr);
  second(pair) = (uintptr_t)consPtr;
  return res;
}

#else /*PROFILING*/

uintptr_t
real_listProf(Region pairRho, Region realRho, long pPoint) {
  double elem;
  uintptr_t *realPtr, *pair, *consPtr, *temp_pair, i, res;

  if (is_inf_and_atbot(pairRho))
    resetRegion(pairRho);
  if (is_inf_and_atbot(realRho))
    resetRegion(realRho);

  /* First element is special because we have to return
   * a pointer to it. */
  elem = 2.5;
  allocRealProf(realRho, realPtr, pPoint);
  convertRealToML(elem, realPtr);
  allocRecordMLProf(pairRho, 2, pair, pPoint);
  first(pair) = (uintptr_t) realPtr;
  makeCONS(pair, consPtr);
  res = (uintptr_t) consPtr;
  for(i=1;i<10;i++) {
    elem = elem + 2.5;
    allocRealProf(realRho, realPtr, pPoint);
    convertRealToML(elem, realPtr);
    allocRecordMLProf(pairRho, 2, temp_pair, pPoint);
    first(temp_pair) = (uintptr_t) realPtr;
    makeCONS(temp_pair, consPtr);
    second(pair) = (uintptr_t) consPtr;
    pair = temp_pair;
  }
  makeNIL(consPtr);
  second(pair) = (uintptr_t)consPtr;
  return res;
}

#endif /*PROFILING*/

/* The function dir returns a list of strings with the file names in
 * directory mlDirName. If the directory can not be opened, then
 * exception exn is raised. The file names in the directory is
 * returned in a list of strings. The list is constructed backwards
 * with the following algorithm:
 *        makeNIL(resList);
 *        while (more elements) {
 *         ml_elem = ...;
 *         allocRecordML(pairRho, 2, pair);
 *         first(pair) = (long) ml_elem;
 *         second(pair) = (long) resList;
 *         makeCONS(pair, resList);
 *        }
 *        return (long) resList;
 * The algorithm is explained in the manual ``Programming With Regions
 * in the MLKit'' found in the doc directory. We have also made a
 * profiling version of the function. Note, that we also try to reset
 * the two infinite regions. */

#ifndef PROFILING
uintptr_t
dir(Region pairRho, Region strRho, Context ctx, String mlDirName, uintptr_t exn) {
  char cDirName[1000];
  uintptr_t *resList, *pairPtr;
  StringDesc *mlStr;
  struct dirent *dp;            /* Dir stream pointer.  */
  DIR *dfd;                     /* Dir file descriptor. */

  convertStringToC(ctx, mlDirName, cDirName, 1000, exn);  /* MEMO: Oops - we should return if the exn-flag is set! */

  if (is_inf_and_atbot(pairRho))
    resetRegion(pairRho);
  if (is_inf_and_atbot(strRho))
    resetRegion(strRho);

  if ((dfd = opendir(cDirName)) == NULL) {
    raise_exn(ctx, exn);
    return 0;
  }
  makeNIL(resList);
  while ((dp = readdir(dfd)) != NULL) {
    mlStr = convertStringToML(strRho, dp->d_name);
    allocRecordML(pairRho, 2, pairPtr);
    first(pairPtr) = (uintptr_t) mlStr;
    second(pairPtr) = (uintptr_t) resList;
    makeCONS(pairPtr, resList);
  }

  (void) closedir(dfd);
  return (uintptr_t) resList;
}

#else /*PROFILING*/

uintptr_t
dirProf(Region pairRho, Region strRho, Context ctx, String mlDirName, uintptr_t exn, long pPoint) {
  char cDirName[1000];
  uintptr_t *resList, *pairPtr;
  StringDesc *mlStr;
  struct dirent *dp;            /* Dir stream pointer.  */
  DIR *dfd;                     /* Dir file descriptor. */

  if (is_inf_and_atbot(pairRho))
    resetRegion(pairRho);
  if (is_inf_and_atbot(strRho))
    resetRegion(strRho);

  convertStringToC(ctx, mlDirName, cDirName, 1000, exn);
  if ((dfd = opendir(cDirName)) == NULL) {
    raise_exn(ctx, exn);
    return 0;
  }

  makeNIL(resList);
  while ((dp = readdir(dfd)) != NULL) {
    mlStr = convertStringToMLProf(strRho, dp->d_name, pPoint);
    allocRecordMLProf(pairRho, 2, pairPtr, pPoint);
    first(pairPtr) = (uintptr_t) mlStr;
    second(pairPtr) = (uintptr_t) resList;
    makeCONS(pairPtr, resList);
  }

  (void) closedir(dfd);
  return (uintptr_t) resList;
}

#endif /*PROFILING*/

#ifndef PROFILING
uintptr_t
change_elem(uintptr_t newPair, Region stringRho, Context ctx, uintptr_t pair, uintptr_t exn) {
  long firstElem_ml, secondElem_ml;
  char cStr[1000];

  firstElem_ml = elemRecordML(pair, 0);
  secondElem_ml = elemRecordML(pair, 1);

  convertStringToC(ctx, (StringDesc *)secondElem_ml, cStr, 1000, exn);
  secondElem_ml = (uintptr_t) convertStringToML(stringRho, cStr);

  elemRecordML(newPair, 0) = secondElem_ml;
  elemRecordML(newPair, 1) = firstElem_ml;

  return newPair;
}
#else
uintptr_t
change_elemProf(uintptr_t newPair, Region stringRho, Context ctx, uintptr_t pair, uintptr_t exn, long pPoint) {
  long firstElem_ml, secondElem_ml;
  char cStr[1000];

  firstElem_ml = elemRecordML(pair, 0);
  secondElem_ml = elemRecordML(pair, 1);

  convertStringToC(ctx, (StringDesc *)secondElem_ml, cStr, 1000, exn);
  secondElem_ml = (uintptr_t) convertStringToMLProf(stringRho, cStr, pPoint);

  elemRecordML(newPair, 0) = secondElem_ml;
  elemRecordML(newPair, 1) = firstElem_ml;

  return newPair;
}
#endif // PROFILING
