#include <sys/types.h>
#include <dirent.h>
#include "MlConvert.h"

/* On HPUX, this file is compiled with:                             */
/*   cc -Aa -D_HPUX_SOURCE -o my_lib.o -c my_lib.c                  */
/* and                                                              */
/*   cc -Aa -D_HPUX_SOURCE -DPROFILING -o my_lib_prof.o -c my_lib.c */
/* when profiling is enabled.                                       */

/* On SUN_OS4, this file is compiled with:                          */
/*   gcc -ansi -o my_lib.o -c my_lib.c                              */
/* and                                                              */
/*   gcc -ansi -o my_lib_prof.o -DPROFILING -c my_lib.c             */
/* when profiling is enabled.                                       */

/* If you also define the flag DEBUG, then various print statements */
/* will be executed in the C functions.                             */

/* In this version of power we have inserted explicit conversion of */
/* integers even though it is not necessary, because the converter  */
/* functions are the identity map.                                  */
/* For portability, it is a good idea to use the converter          */
/* functions.                                                       */
/* The function also works when profiling is enabled.               */
int power(int base, int n) {
  int p;

#ifdef DEBUG
  printf("Now in power function\n");
#endif /*DEBUG*/

  base = convertIntToC(base);
  n = convertIntToC(n);

  for (p = 1; n > 0; --n)
    p = p * base;

#ifdef DEBUG
  printf("power(%d,%d) = %d\n",base,n,p);
  printf("Now exitting C function.\n");
#endif /*DEBUG*/

  p = convertIntToML(p);
  return p;
}

/* This version of the power function assume that auto convertion   */
/* is used.                                                         */
int power_auto(int base, int n) {
  int p;
  for (p = 1; n > 0; --n)
    p = p * base;
  return p;
}

/* A variant of the power function where the base is a float.       */
/* The base and res are boxed, i.e. they are pointers to            */
/* pre-allocated space. We retrieve the real from base and put the  */
/* result in memory pointed at by res.                              */
/* The function also works when profiling is enabled.               */
int power_real(int res, int base, int n) {
  double p, base_real;

  base_real = convertRealToC(base);

#ifdef DEBUG
  printf("Now in power_real function, with base %3.1f\n", base_real);
#endif /*DEBUG*/

  n = convertIntToC(n);

  for (p = 1; n > 0; --n)
    p = p * base_real;

#ifdef DEBUG
  printf("power(%3.1f,%d) = %3.1f\n",base_real,n,p);
  printf("Now exitting C function.\n");
#endif /*DEBUG*/

  convertRealToML(p,res);

  return res;
}

/* A variant of power where an exception may be raised. */
/* We raise exn when base is negative.                  */
/* The function also works when profiling is enabled.   */
int power_exn(int res, int base, int n, int exn) {
  double p, base_real;

  base_real = convertRealToC(base);

#ifdef DEBUG
  printf("Now in power_exn function, with base %3.1f\n", base_real);
#endif /*DEBUG*/

  if (base_real < 0.0) {
    printf("Now raising exception.\n");
    raise_exn(exn);
    return; /* This return statement is necessary on SUN_OS4 */
  }

  n = convertIntToC(n);

  for (p = 1; n > 0; --n)
    p = p * base_real;

#ifdef DEBUG
  printf("power(%3.1f,%d) = %3.1f\n",base_real,n,p);
  printf("Now exitting C function.\n");
#endif /*DEBUG*/

  convertRealToML(p,res);

  return res;
}

/* This example shows how to traverse a list.               */
/* We run through the list while the next element           */
/* in the list is a CONS.                                   */
/* Example of the string list representation:               */
/*  ["ABC","DEF","GHI","JKL"] =                             */
/*      CONS("ABC",CONS("DEF",CONS("GHI",CONS("JKL",NIL)))) */
/* Works also when profiling is enabled.                    */
int print_string_list(int strs) {
  int ys;

#ifdef DEBUG
  printf("Now in C function print_string.\n");
#endif /*DEBUG*/

  for (ys=strs; isCONS(ys); ys=tl(ys))
    printString((StringDesc *) hd(ys));

#ifdef DEBUG
  printf("Now exitting C function print_string.\n");
#endif /*DEBUG*/

  return;
}


/* The function real_list returns a list of reals.          */
/* The list is constructed forwards.                        */
/* The function allocates memory in infinite regions, thus, */
/* we have also made a version for profiling.               */
/* The algorithm for constructing a list forwards are as    */
/* follows:                                                 */
/* The first element is special because we have to return   */
/* a pointer to it.                                         */
/*        ml_elem = ...                                     */
/*        allocRecordML(pairRho, 2, pair);                  */
/*        first(pair) = (int) ml_elem;                      */
/*        makeCONS(consRho, pair, cons);                    */
/*        res = (int) cons;                                 */
/*                                                          */
/*        while (more elements) {                           */
/*          ml_elem = ...                                   */
/*          allocRecordML(pairRho, 2, temp_pair);           */
/*          first(temp_pair) = (int) ml_elem;               */
/*          makeCONS(consRho, temp_pair, cons);             */
/*          second(pair) = (int) cons;                      */
/*          pair = temp_pair;                               */
/*        }                                                 */
/*        makeNIL(consRho, cons);                           */
/*        second(pair) = (int)cons;                         */
/*        return res;                                       */
/* The algorithm is explained in the book-let:              */
/*     Programming With Regions in the ML Kit               */
/* found in the doc directory.                              */
/* Note, that we also _try_ to reset the three infinite     */
/* regions.                                                 */
#ifndef PROFILING
int real_list(int consRho, int pairRho, int realRho) {
  double elem;
  int *realPtr, *pair, *consPtr, *temp_pair, i, res;

#ifdef DEBUG
  printf("Now constructing a list with 10 reals.\n");
#endif /*DEBUG*/

  if (is_inf_and_atbot(consRho))
    resetRegion(consRho);
  if (is_inf_and_atbot(pairRho))
    resetRegion(pairRho);
  if (is_inf_and_atbot(realRho))
    resetRegion(realRho);

  /* First element is special because we have to return
     a pointer to it.                                   */
  elem = 2.5;
  allocReal(realRho, realPtr);
  convertRealToML(elem, realPtr);
  allocRecordML(pairRho, 2, pair);
  first(pair) = (int) realPtr;
  makeCONS(consRho, pair, consPtr);
  res = (int) consPtr;
  for(i=1;i<10;i++) {
    elem = elem + 2.5;
    allocReal(realRho, realPtr);
    convertRealToML(elem, realPtr);
    allocRecordML(pairRho, 2, temp_pair);
    first(temp_pair) = (int) realPtr;
    makeCONS(consRho, temp_pair, consPtr);
    second(pair) = (int) consPtr;
    pair = temp_pair;
  }
  makeNIL(consRho, consPtr);
  second(pair) = (int)consPtr;

  return res;
}

#else /*PROFILING*/

int real_listProf(int consRho, int pairRho, int realRho, int pPoint) {
  double elem;
  int *realPtr, *pair, *consPtr, *temp_pair, i, res;

#ifdef DEBUG
  printf("Now constructing a list with 10 reals.\n");
#endif /*DEBUG*/

  if (is_inf_and_atbot(consRho))
    resetRegion(consRho);
  if (is_inf_and_atbot(pairRho))
    resetRegion(pairRho);
  if (is_inf_and_atbot(realRho))
    resetRegion(realRho);

  /* First element is special because we have to return
     a pointer to it.                                   */
  elem = 2.5;
  allocRealProf(realRho, realPtr, pPoint);
  convertRealToML(elem, realPtr);
  allocRecordMLProf(pairRho, 2, pair, pPoint);
  first(pair) = (int) realPtr;
  makeCONSProf(consRho, pair, consPtr, pPoint);
  res = (int) consPtr;
  for(i=1;i<10;i++) {
    elem = elem + 2.5;
    allocRealProf(realRho, realPtr, pPoint);
    convertRealToML(elem, realPtr);
    allocRecordMLProf(pairRho, 2, temp_pair, pPoint);
    first(temp_pair) = (int) realPtr;
    makeCONSProf(consRho, temp_pair, consPtr, pPoint);
    second(pair) = (int) consPtr;
    pair = temp_pair;
  }
  makeNILProf(consRho, consPtr, pPoint);
  second(pair) = (int)consPtr;

  return res;
}

#endif /*PROFILING*/

/* The function dir returns a list of strings with the file */
/* names in directory mlDirName.                            */
/* If the directory can not be opened, then exception exn   */
/* is raised.                                               */
/* The file names in the directory is returned in a list of */
/* strings. The list is constructed backwards with the      */
/* following algorithm:                                     */
/*        makeNIL(consRho,resList);                         */
/*        while (more elements) {                           */
/*        ml_elem = ...;                                    */
/*        allocRecordML(pairRho, 2, pair);                  */
/*        first(pair) = (int) ml_elem;                      */
/*        second(pair) = (int) resList;                     */
/*        makeCONS(consRho, pair, resList);                 */
/*        }                                                 */
/*        return (int) resList;                             */
/* The algorithm is explained in the book-let:              */
/*     Programming With Regions in the ML Kit               */
/* found in the doc directory.                              */
/* We have also made a profiling version of the function.   */
/* Note, that we also _try_ to reset the three infinite     */
/* regions.                                                 */
#ifndef PROFILING

int dir(int consRho, int pairRho, int strRho, StringDesc *mlDirName, int exn) {
  char cDirName[1000];
  int *resList, *pairPtr;
  StringDesc *mlStr;
  struct dirent *dp;            /* Dir stream pointer.  */
  DIR *dfd;                     /* Dir file descriptor. */

  if (is_inf_and_atbot(consRho))
    resetRegion(consRho);
  if (is_inf_and_atbot(pairRho))
    resetRegion(pairRho);
  if (is_inf_and_atbot(strRho))
    resetRegion(strRho);

#ifdef DEBUG
  printf("\nIs in dir with arg: ");
  printString(mlDirName);
  printf(".\n");
#endif /*DEBUG*/

  convertStringToC(mlDirName, cDirName, 1000);

  if ((dfd = opendir(cDirName)) == NULL) {
    raise_exn(exn);
    return; /* This return statement is necessary on SUN_OS4 */
  }
  makeNIL(consRho,resList);  
  while ((dp = readdir(dfd)) != NULL) {
    mlStr = convertStringToML(strRho, dp->d_name);
    allocRecordML(pairRho, 2, pairPtr);
    first(pairPtr) = (int) mlStr;
    second(pairPtr) = (int) resList;
    makeCONS(consRho, pairPtr, resList);
  }
   
  (void) closedir(dfd);

#ifdef DEBUG
  printf("\nExit from dir\n");
#endif /*DEBUG*/

  return (int) resList;
}

#else

int dirProf(int consRho, int pairRho, int strRho, StringDesc *mlDirName, int exn, int pPoint) {
  char cDirName[1000];
  int *resList, *pairPtr;
  StringDesc *mlStr;
  struct dirent *dp;            /* Dir stream pointer.  */
  DIR *dfd;                     /* Dir file descriptor. */

  if (is_inf_and_atbot(consRho))
    resetRegion(consRho);
  if (is_inf_and_atbot(pairRho))
    resetRegion(pairRho);
  if (is_inf_and_atbot(strRho))
    resetRegion(strRho);

#ifdef DEBUG
  printf("\nIs in dir with arg: ");
  printString(mlDirName);
  printf(".\n");
#endif /*DEBUG*/

  convertStringToC(mlDirName, cDirName, 1000);
  if ((dfd = opendir(cDirName)) == NULL) {
    raise_exn(exn);
    return; /* This return statement is necessary on SUN_OS4 */
  }

  makeNILProf(consRho,resList,pPoint);  
  while ((dp = readdir(dfd)) != NULL) {
    mlStr = convertStringToMLProfiling(strRho, dp->d_name, pPoint);
    allocRecordMLProf(pairRho, 2, pairPtr, pPoint);
    first(pairPtr) = (int) mlStr;
    second(pairPtr) = (int) resList;
    makeCONSProf(consRho, pairPtr, resList, pPoint);
  }
   
  (void) closedir(dfd);

#ifdef DEBUG
  printf("\nExit from dir\n");
#endif /*DEBUG*/

  return (int) resList;
}

#endif /*PROFILING*/

int change_elem(int newPair, int stringRho, int pair) {
  int firstElem_ml, secondElem_ml;
  char cStr[1000];

  firstElem_ml = elemRecordML(pair, 0);
  secondElem_ml = elemRecordML(pair, 1);

  convertStringToC((StringDesc *)secondElem_ml, cStr, 1000);
  secondElem_ml = (int) convertStringToML(stringRho, cStr);

  elemRecordML(newPair, 0) = secondElem_ml;
  elemRecordML(newPair, 1) = firstElem_ml;

  return newPair;
}
