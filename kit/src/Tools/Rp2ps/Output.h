#ifndef OUTPUT
#define OUTPUT
#include <stdio.h>
#include "Flags.h"

extern double areabelow;

/***************************************************************
 * Graph and page declarations.                                *
 ***************************************************************/
#define TITLE_FONT             0
#define SCALE_FONT             1
#define KEY_FONT               2
#define SMALLKEY_FONT          3

#define LARGE_FONT            12  /* Helvetica 12pt                        */
#define NORMAL_FONT           10  /* Helvetica 10pt                        */
#define SMALL_FONT             6  /* Helvetica  6pt                        */

#define BORDER_HEIGHT        432  /* page border box 432pt (6 inches high) */
#define BORDER_WIDTH         648  /* page border box 648pt (9 inches wide) */
#define BORDER_SPACE           5  /* page border space                      */
#define BORDER_RADIUS          8  /* corner radius is 8pt                  */

#define LINE_THICK    (float)0.5  /* page border line thickness 0.5pt      */

#define TITLE_HEIGHT          20    /* title box is 20pt high                 */
#define TITLE_RADIUS           4    /* corner radius is 8pt                  */
#define TITLE_TEXT_SPACE       6    /* space between title text and box      */

#define AXIS_THICK      (float)0.5  /* axis thickness 0.5pt                  */
#define AXIS_TEXT_SPACE          6  /* space between axis legends and axis   */
#define AXIS_TEXT_FONT NORMAL_FONT  /* axis legends in normal font           */
#define AXIS_Y_TEXT_SPACE       35  /* space for y axis text                 */

#define KEY_BOX_WIDTH           14  /* key boxes are 14pt high               */

#define SMALL_JOB_STRING_WIDTH  35  /* small title for 35 characters or less */
#define BIG_JOB_STRING_WIDTH    80  /* big title for everything else         */

#define GRAPH_X0        (AXIS_Y_TEXT_SPACE + (2 * BORDER_SPACE))
#define GRAPH_Y0        (AXIS_TEXT_FONT + (2 * BORDER_SPACE))

#define FUDGE 1.7

#define START_X  72     /* start  72pt (1 inch)   from left   (portrait)  */
#define START_Y 108     /* start 108pt (1.5 inch) from bottom (portrait)  */
#define VERSION "0.01"  /* as of 09/11/93        */

extern double borderheight;
extern double borderwidth;
extern double borderspace;
/*double borderradius;*/

extern double titlewidth;
extern double titleheight;
extern double titleradius;
extern double titletextspace;

extern double linethick;

extern double graphx0;
extern double graphy0;

typedef enum {JustifyLeft,JustifyRight,JustifyCenter,JustifyVertical} Justify;

typedef enum {Closed,Open} Kind;

typedef struct {
 char *suffix;
 void (*Prelude)(double,int);
 void (*Prologue)(void);
 void (*Scale)(double scale);
 void (*Landscape)(void);
 void (*Portrait)(void);
 void (*Fonts)(void);
 void (*Text)(Justify,double,double,int,char *);
 void (*PathNew)(Kind,int);
 void (*PathMoveTo)(double,double);
 void (*PathLineTo)(double,double);
 void (*PathLine)(double,double);
 void (*PathFill)(int);
 void (*PathWidth)(double);
 void (*PathStroke)(void);

} Format;

extern Format *output;     /* Pointer to the chosen format functions.    */
extern int fonttab[];
double StringSize(char *s);

void BorderOutlineBox(void);
void TitleOutlineBox(void);
void TitleText(void);
void Prologue(void);
void outputBox(int fill,double x,double y,double w,double h);
void outputLine(double x,double y,double dx,double dy);
void outputLineTo(double x,double y,double x2,double y2);
void outputCaret(double x,double y,double d);
int FontSize(int font);

#endif /* OUTPUT */
