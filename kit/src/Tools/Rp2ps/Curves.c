#include "Flags.h"
#include "Curves.h"
#include "Output.h"
#include "Rp2Ps.h"

/***************************************************************************
 * Curves.                                                                 *
 ***************************************************************************/
double graphheight;
double graphwidth;

double xrange;
double yrange;

static double* x;                                /* x and y values  */
static double* y;
static double* py;                               /* previous y values */

void CurvesInit(void)
{
  int i;
  double sf;

  x  =  (double*) xmalloc(nsamples * sizeof(double));
  y  =  (double*) xmalloc(nsamples * sizeof(double));
  py =  (double*) xmalloc(nsamples * sizeof(double));

  sf = graphwidth / xrange; /*16/11/1995, Niels*/

  /*printf("CurvesInit, sf: %8.6f, graphwidth: %8.2f, xrange: %8.2f\n", sf, graphwidth, xrange);*/
  
  for (i = 0; i < nsamples; i++) {
    x [ i ] = (sampletable[ i ] - sampletable[ 0 ]) * sf; /*- sampletable[ 0 ] indsat den 16/11/1995, Niels */
    y [ i ] = 0;
    py[ i ] = 0;
  }
}

/*
 *      Map virtual x coord to physical x coord
 */
static double xpage(double x)
{
  return (x + graphx0);
}

/*
 *      Map virtual y coord to physical y coord
 */
static double ypage(double y)
{
  return (y + graphy0);
}

static void PlotCurveLeftToRight(double *x, double *y)
{
  int i;

  for (i = 0; i < nsamples; i++)
    output->PathLineTo(xpage(x[i]), ypage(y[i]));
}

static void PlotCurveRightToLeft(double *x, double *y)
{
  int i;

  for (i = nsamples - 1; i >= 0; i-- )
    output->PathLineTo(xpage(x[i]), ypage(y[i]));
}

/*
 *      Save the curve coordinates stored in y[] in py[].
 */
static void SaveCurve(double* y, double* py)
{
  int i;

  for (i = 0; i < nsamples; i++)
    py[i] = y[i];
}

static void ShadeCurve(int no, double *x, double *y, double *py)
{
  output->PathNew(Closed,2*nsamples);
  output->PathFill(no);
  output->PathWidth(0.1);
  output->PathMoveTo(xpage(x[0]), ypage(py[0]));
  PlotCurveLeftToRight(x, py);
  output->PathLineTo(xpage(x[nsamples - 1]), ypage(y[nsamples - 1]));
  PlotCurveRightToLeft(x, y);
  output->PathStroke();

  SaveCurve(y, py);
}

/*
 * Curve() -- Draw a curve, and fill the area that is below it and above
 *            the previous curve.
 */
static void Curve(int no,ENTRYPTR e)
{
  SAMPLEPTR s;

  for (s = e->samples; s; s = s->next)
        y[ s->n ] += s->nbytes;

  ShadeCurve(no,x, y, py);
}

/*
 * Draw vertical line at sample points
 */
static void SampleLine(double x, double y1, double y2)
{
    outputLineTo(xpage(x),ypage(y1),xpage(x),ypage(y2));
}

static void Samples(void)
{
  int i;
  for (i = 0; i < nsamples; i++) {
    SampleLine(x[i],py[i],graphheight);
  }
}

static void SmallSamples(void)
{
  int i;
  for (i = 0; i < nsamples; i++) {
    SampleLine(x[i],0,-3);
  }
}

void Curves(void)
{
  int i;

  for (i = 0; i < nidents; i++) {
    Curve(i,identtable[i]);
  }
  if (yflag)
    Samples();
  else
    SmallSamples();
}

void Comments(void)
{
  int i;
  double m;
  for (i = 0; i < ncomments; i++) {
    m = ((double)(commenttable[i]-sampletable[0]) / xrange) * graphwidth; /*16/11/1995, Niels*/
    SampleLine(m,0.0,graphheight-10);
    output->Text(JustifyVertical,xpage(m),ypage(0.0)+graphheight-10,SCALE_FONT,commentstring[i]);
  }
}

void Marks(void)
{
  int i;
  double m;

  for (i = 0; i < nmarks; i++) {
    m = ((double)(marktable[i]-sampletable[0]) / xrange) * graphwidth; /*16/11/1995, Niels*/
    outputCaret(xpage(m), ypage(0.0), 4.0);
  }
}

/***************************************************************************
 * Key; put identifiers on the graph.                                      *
 ***************************************************************************/
static void KeyEntry(double centreline, char* name, int colour)
{
  double namebase;
  double keyboxbase;
  double kstart;

  namebase = centreline - (double) (NORMAL_FONT / 2);
  keyboxbase = centreline - ((double) KEY_BOX_WIDTH / 2);
  
  kstart = graphx0 + graphwidth;

  #if DEBUG_KEYENTRY
  printf("KeyEntry, colour: %5d, kstart: %8.4f, borderspace: %8.4f, keyboxbase: %8.4f, KEY_BOX_WIDTH: %5d, KEY_BOX_WIDTH: %5d\n",
	 colour, kstart, borderspace, keyboxbase, KEY_BOX_WIDTH, KEY_BOX_WIDTH);
  #endif

  outputBox(colour,kstart + borderspace, keyboxbase, KEY_BOX_WIDTH, KEY_BOX_WIDTH);

  output->Text(JustifyLeft
	       ,kstart + (double) KEY_BOX_WIDTH + 2 * borderspace, namebase
	       ,KEY_FONT
	       ,name);
}

void Key(void)
{
  int i;
  double c;
  double dc;
  ENTRYPTR e;

  c  = graphy0;
  dc = graphheight / ((double)nidents + 1);

  /*printf("Key, c: %8.4f, dc: %8.4f\n", c, dc);*/

  for (i = 0; i < nidents; i++) {
    c += dc;
    e = identtable[ i ];
    KeyEntry(c, e->name, i);
  }
}

/***************************************************************************
 * Axes                                                                    *
 ***************************************************************************/
typedef enum {MEGABYTE, KILOBYTE, BYTE} mkb; 

/*
 *      Find a "nice round" value to use on the axis.
 */
static double Round(double y)
{
  int i;

  for (i = 0; y > (double)10.0e-3; y /= 10, i++)
    ;

  if (y > 4.0e-3) {
    y = (double)5.0e-3;
  } else if (y > 1.0e-3) {
    y = (double)2.0e-3;
  } else {
    y = (double)1.0e-3;
  }   

  for ( ; i > 0; y = y * 10, i--)
    ;

  return y;
}

static void XAxisMark(double x, double num)
{
  char info[100];
  /* calibration mark */
  outputLine(xpage(x), ypage(0.0), 0.0, -5.0);

  sprintf(info,"%.1f",num);
  output->Text(JustifyCenter,xpage(x),borderspace,SCALE_FONT,info);
}

#define N_X_MARKS       7
#define XFUDGE          15      

static void XAxis(void)
{
  double increment, i; 
  double t, x;
  double legendlen;
  double aBitMore = 15.0;
 
  /* draw the x axis line */
  outputLine(xpage(0.0), ypage(0.0), graphwidth,0);

    /* draw x axis legend */
  if (useTickNo) {
    output->Text(JustifyLeft,xpage(0.0) + graphwidth+aBitMore, borderspace,SCALE_FONT,"ticks"); /* 21/12/1995, Niels aBitMore inserted. */
  }
  else {
    output->Text(JustifyLeft,xpage(0.0) + graphwidth+aBitMore, borderspace,SCALE_FONT,"seconds");
  }
    /* draw x axis scaling */

  increment = Round(xrange / (double) N_X_MARKS); /*16/11/1995, Niels*/

  if (increment < 0.01) increment = 0.01;

  t = graphwidth / xrange; /*16/11/1995, Niels*/
  legendlen = StringSize("seconds") + (double) XFUDGE;
 
  for (i = sampletable[ 0 ]; i < sampletable[ nsamples - 1]; i += increment) {
    x = (i - sampletable[ 0 ]) * t;  /* niels-- -sampletable[0] goer at startx er den foerste sampletime. */
 
    if (x < (graphwidth/* - legendlen*/)) {  /*commented-17/11/1995, Niels*/
      XAxisMark(x,i);
    } 
  } 
}

static void YAxisMark(double y, double num, mkb unit)
{
  char info[100];

    /* calibration mark */
  outputLine(xpage(0.0), ypage(y), -4.0, 0.0);
 
    /* number */

  switch (unit) {
    case MEGABYTE :
      sprintf(info, "%dM", (int) (num / 1e6));
      break;
    case KILOBYTE :
      sprintf(info, "%dk", (int) (num / 1e3));
      break;
    case BYTE:
      sprintf(info, "%d", (int) (num));
      break;
  }

  output->Text(JustifyRight,graphx0 - borderspace,ypage(y),SCALE_FONT,info);
}

#define N_Y_MARKS        7      
#define YFUDGE          15 

static void YAxis(void)
{
  double increment, i;
  double t, y;
  double legendlen;
  mkb unit;
  
    /* draw the y axis line */
  outputLine(xpage(0.0), ypage(0.0), 0.0, graphheight);

  /* draw y axis legend */

  output->Text(JustifyVertical,xpage(0.0) - borderspace,ypage(0.0)+graphheight,SCALE_FONT,yLab); /*ylab was bytes 17/11/1995, Niels*/

    /* draw y axis scaling */
  increment = Round(yrange / (double) N_Y_MARKS);

  if (increment < (double)1.0) increment = (double)1.0;

  if (increment >= 1e6) {
      unit = MEGABYTE;
  } else if (increment >= 1e3) {
      unit = KILOBYTE;
  } else {
      unit = BYTE;
  }   

  t = graphheight / yrange; 
  legendlen = StringSize(yLab) + (double) YFUDGE; /*ylab was bytes 17/11/1995, Niels*/

  for (i = 0; i <= yrange; i += increment) {
    y = i * t;

    if (y < (graphheight - legendlen)) {
      YAxisMark(y, i, unit);
    }
  } 
}

void Axes(void)
{
    XAxis();
    YAxis();
}

/* drawMaxValue, draws a horizontal line showing a maximum */
/* on the graph.                                           */
void drawMaxValue(float value, char *valStr) {
  
  double strSize;
  double xCor, yCor;
  
  strSize = StringSize(valStr);
  xCor = (graphwidth-strSize)/2.0;
  yCor = graphheight/yrange*value;
  outputLineTo(xpage(0.0)+(graphwidth*0.1), ypage(0.0)+yCor, xpage(0.0)+xCor-4, ypage(0.0)+yCor);
  output->Text(JustifyLeft,xpage(0.0)+xCor, ypage(0.0)+yCor-5,SCALE_FONT,valStr);
  xCor += strSize;
  outputLineTo(xpage(0.0)+xCor+4, ypage(0.0)+yCor, xpage(0.0)+(graphwidth*0.9), ypage(0.0)+yCor);

  return;
}
