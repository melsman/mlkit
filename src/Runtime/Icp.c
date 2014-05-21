/*----------------------------------------------------------------*
 *          Instruction Count Profiling                           *
 *----------------------------------------------------------------*/

#include <stdio.h>

/*------------------------------------------------------------------*
 *                 Runtime system for icp.                          *
 *                                                                  *
 * print_icp_result: Print number of instructions executed.         *
 *------------------------------------------------------------------*/

void print_icp_result(int iCount) {
  long base = -2147483647; /* -(2^31 - 1) */
  long val = iCount;
  printf("Result of Instruction Count Profiling: %ld.\n", val - base);
  return;
}
