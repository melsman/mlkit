#include <stdio.h>
#include "Rp2Ps.h"

/***************************************************************************
 * Functions for error handling.                                           *
 ***************************************************************************/

void Disaster(char* format)
{
  fprintf(stderr, "%s Disatser! :", programname);
  fprintf(stderr, "    %s\n", format);
  exit(1);
}
