/* qualify-name.c
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 */

#include <string.h>
#include "ml-base.h"
#include "machine-id.h"

#define SUFFIX	MACHINE_ID "-" OPSYS_ID

/* QualifyImageName:
 *
 * Given a pathname for an image file, this adds the architecture extension
 * to the pathname (if it doesn't already have it).  It returns TRUE, if the
 * extension was added.
 */
bool_t QualifyImageName (char *buf)
{
    int		len = strlen(buf);
    int		midLen = sizeof(SUFFIX); /* length of ID + 1 */

    if ((midLen+1 < len) && (buf[len-midLen] == '.')
    && (strcmp(&(buf[len-(midLen-1)]), SUFFIX) == 0))
      /* the pathname is already qualified by the machine ID and OPSYS */
	return FALSE;

    strcat (buf, "." SUFFIX);

    return TRUE;

} /* end of QualifyImageName */

