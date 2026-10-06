/* Legacy marking primitive retained as a bridge to the rpview stream.
 * All snapshot collection and attribution live in RegionProfile.c. */
#include "RegionProfile.h"
#include "String.h"
void queueMarkProf(String text, long point) {
  (void)point;
  mlkit_rp_mark(text);
}
