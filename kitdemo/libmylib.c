#include <sys/types.h>
#include <dirent.h>
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

int power(int base, int n) {
  int p;
  base = convertIntToC(base);
  n = convertIntToC(n);
  for (p = 1; n > 0; --n) p = p * base;
  p = convertIntToML(p);
  return p;
}

/* This version of the power function assumes that auto convertion
 * is used. */
int power_auto(int base, int n) {
  int p;
  for (p = 1; n > 0; --n) p = p * base;
  return p;
}

/* A variant of the power function where the base is a float. The base
 * and res are boxed, i.e. they are pointers to pre-allocated
 * space. We retrieve the real from base and put the result in memory
 * pointed at by res. The function also works when profiling is
 * enabled. */
int power_real(int res, int base, int n) {
  double p, base_real;
  base_real = convertRealToC(base);
  n = convertIntToC(n);
  for (p = 1; n > 0; --n) p = p * base_real;
  convertRealToML(p,res);
  return res;
}

/* A variant of power where an exception may be raised. We raise exn
 * when base is negative. The function also works when profiling is
 * enabled. */
int power_exn(int res, int base, int n, int exn) {
  double p, base_real;
  base_real = convertRealToC(base);
  if (base_real < 0.0) {
    printf("Now raising exception.\n");
    raise_exn(exn);
    return 0; /* This return statement is necessary
	       * with the C backend */
  }
  n = convertIntToC(n);
  for (p = 1; n > 0; --n) p = p * base_real;
  convertRealToML(p,res);
  return res;
}

/* This example shows how to traverse a list.  We run through the list
 * while the next element in the list is a CONS. Example of the string
 * list representation: ["ABC","DEF","GHI","JKL"] =
 * CONS("ABC",CONS("DEF",CONS("GHI",CONS("JKL",NIL)))). Works also
 * when profiling is enabled. */
int print_string_list(int strs) {
  int ys;
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
 *        first(pair) = (int) ml_elem;
 *        makeCONS(pair, cons);
 *        res = (int) cons;
 *
 *        while (more elements) {
 *          ml_elem = ...
 *          allocRecordML(pairRho, 2, temp_pair);
 *          first(temp_pair) = (int) ml_elem;
 *          makeCONS(temp_pair, cons);
 *          second(pair) = (int) cons;
 *          pair = temp_pair;
 *        }
 *        makeNIL(cons);
 *        second(pair) = (int)cons;
 *        return res;
 * The algorithm is explained in the manual ``Programming With Regions
 * in the MLKit'' found in the doc directory. Note, that we also try
 * to reset the two infinite regions. */

#ifndef PROFILING
int real_list(Region pairRho, Region realRho) {
  double elem;
  int *realPtr, *pair, *consPtr, *temp_pair, i, res;

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
  first(pair) = (int) realPtr;
  makeCONS(pair, consPtr);
  res = (int) consPtr;
  for(i=1;i<10;i++) {
    elem = elem + 2.5;
    allocReal(realRho, realPtr);
    convertRealToML(elem, realPtr);
    allocRecordML(pairRho, 2, temp_pair);
    first(temp_pair) = (int) realPtr;
    makeCONS(temp_pair, consPtr);
    second(pair) = (int) consPtr;
    pair = temp_pair;
  }
  makeNIL(consPtr);
  second(pair) = (int)consPtr;
  return res;
}

#else /*PROFILING*/

int real_listProf(Region pairRho, Region realRho, int pPoint) {
  double elem;
  int *realPtr, *pair, *consPtr, *temp_pair, i, res;

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
  first(pair) = (int) realPtr;
  makeCONS(pair, consPtr);
  res = (int) consPtr;
  for(i=1;i<10;i++) {
    elem = elem + 2.5;
    allocRealProf(realRho, realPtr, pPoint);
    convertRealToML(elem, realPtr);
    allocRecordMLProf(pairRho, 2, temp_pair, pPoint);
    first(temp_pair) = (int) realPtr;
    makeCONS(temp_pair, consPtr);
    second(pair) = (int) consPtr;
    pair = temp_pair;
  }
  makeNIL(consPtr);
  second(pair) = (int)consPtr;
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
 *         first(pair) = (int) ml_elem;
 *         second(pair) = (int) resList;
 *         makeCONS(pair, resList);
 *        }
 *        return (int) resList;
 * The algorithm is explained in the manual ``Programming With Regions
 * in the MLKit'' found in the doc directory. We have also made a
 * profiling version of the function. Note, that we also try to reset
 * the two infinite regions. */

#ifndef PROFILING
int dir(Region pairRho, Region strRho, String mlDirName, int exn) {
  char cDirName[1000];
  int *resList, *pairPtr;
  StringDesc *mlStr;
  struct dirent *dp;            /* Dir stream pointer.  */
  DIR *dfd;                     /* Dir file descriptor. */

  convertStringToC(mlDirName, cDirName, 1000, exn);  /* MEMO: Oops - we should return if the exn-flag is set! */

  if (is_inf_and_atbot(pairRho))
    resetRegion(pairRho);
  if (is_inf_and_atbot(strRho))
    resetRegion(strRho);

  if ((dfd = opendir(cDirName)) == NULL) {
    raise_exn(exn);
    return 0;
  }
  makeNIL(resList);
  while ((dp = readdir(dfd)) != NULL) {
    mlStr = convertStringToML(strRho, dp->d_name);
    allocRecordML(pairRho, 2, pairPtr);
    first(pairPtr) = (int) mlStr;
    second(pairPtr) = (int) resList;
    makeCONS(pairPtr, resList);
  }

  (void) closedir(dfd);
  return (int) resList;
}

#else /*PROFILING*/

int dirProf(Region pairRho, Region strRho, String mlDirName, int exn, int pPoint) {
  char cDirName[1000];
  int *resList, *pairPtr;
  StringDesc *mlStr;
  struct dirent *dp;            /* Dir stream pointer.  */
  DIR *dfd;                     /* Dir file descriptor. */

  if (is_inf_and_atbot(pairRho))
    resetRegion(pairRho);
  if (is_inf_and_atbot(strRho))
    resetRegion(strRho);

  convertStringToC(mlDirName, cDirName, 1000, exn);
  if ((dfd = opendir(cDirName)) == NULL) {
    raise_exn(exn);
    return 0;
  }

  makeNIL(resList);
  while ((dp = readdir(dfd)) != NULL) {
    mlStr = convertStringToMLProf(strRho, dp->d_name, pPoint);
    allocRecordMLProf(pairRho, 2, pairPtr, pPoint);
    first(pairPtr) = (int) mlStr;
    second(pairPtr) = (int) resList;
    makeCONS(pairPtr, resList);
  }

  (void) closedir(dfd);
  return (int) resList;
}

#endif /*PROFILING*/

#ifndef PROFILING
int change_elem(int newPair, Region stringRho, int pair, int exn) {
  int firstElem_ml, secondElem_ml;
  char cStr[1000];

  firstElem_ml = elemRecordML(pair, 0);
  secondElem_ml = elemRecordML(pair, 1);

  convertStringToC((StringDesc *)secondElem_ml, cStr, 1000, exn);
  secondElem_ml = (int) convertStringToML(stringRho, cStr);

  elemRecordML(newPair, 0) = secondElem_ml;
  elemRecordML(newPair, 1) = firstElem_ml;

  return newPair;
}
#else
int change_elemProf(int newPair, Region stringRho, int pair, int exn, int pPoint) {
  int firstElem_ml, secondElem_ml;
  char cStr[1000];

  firstElem_ml = elemRecordML(pair, 0);
  secondElem_ml = elemRecordML(pair, 1);

  convertStringToC((StringDesc *)secondElem_ml, cStr, 1000, exn);
  secondElem_ml = (int) convertStringToMLProf(stringRho, cStr, pPoint);

  elemRecordML(newPair, 0) = secondElem_ml;
  elemRecordML(newPair, 1) = firstElem_ml;

  return newPair;
}
#endif // PROFILING
