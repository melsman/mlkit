/***************************************************************
 * Declarations used to construct the graph data structure.    *
 ***************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "Rp2Ps.h"
#include "Flags.h"
#include "Error.h"
#include "Types.h"
#include "Output.h"
#include "Alloc.h"
#include "PostScript.h"
#include "Curves.h"

static double maxDouble (double d1, double d2) {
  if (d1<d2)
    return d2;
  else
    return d1;
}

/***************************************************************************
 * We keep the top noOfBands-1 and then put the rest in the other band.    *
 ***************************************************************************/
/*
 * TopBands() -- We only have room in the key for a maximum of noOfBands identifiers. 
 *               We therefore choose to keep the top noOfBands-1 bands --- these will 
 *               be the most important ones, since this pass is performed 
 *               after the threshold and standard deviation passes. If there 
 *               are more than noOfBands bands, the excess are gathered together as 
 *               an "OTHER" band which appears as band noOfBands.
 */

static void TopBands(void)
{
  int i;
  int j;
  int compact;
  float* other;
  ENTRYPTR e;
  SAMPLEPTR s;
  SAMPLEPTR t;

  #if CHAT
  printf("TopBands -- ENTER\n");
  #endif

  i = nidents;

  if (i <= noOfBands) return;  /* nothing to do     */

  /* build a list of samples for "OTHER" */ 
  compact = (i - noOfBands) + 1;

  other = (float*) xmalloc(nsamples * sizeof(float));

  for (j = 0; j < nsamples; j++) {
    other[ j ] = 0;
  }
  for (i = 0; i < compact; i++) {
    e = identtable[ i ];
    for (s = e->samples; s; s = s->next) {
      other[ s->n ] += s->nbytes;
    }
  }
  s = 0;

  for (i = 0; i < nsamples; i++) {
        t = MallocSample();
        t->n = i;
        t->nbytes = other[ i ];
        t->next = s;
        s = t;
    }   
  e = MallocEntry();
  e->name = "OTHER"; 
  e->samples = s; 
  
  /* slide samples down */
  
  for (i = compact; i < nidents; i++) {
    identtable[ i - compact + 1 ] = identtable[ i ];
  }

  nidents = noOfBands;
  identtable[ 0 ] = e;

  #if CHAT
  printf("TopBands -- LEAVE\n");
  #endif

  return;
}

/***************************************************************************
 * We calculate the dimensions for the graph.                              *
 ***************************************************************************/
/*
 * MaxCombinedHeight() -- Return the maximum combined height that all the 
 *                        sample curves will reach. This (absolute) figure 
 *                        can then be used to scale the samples automatically 
 *                        so that they fit on the page.
 */
static float MaxCombinedHeight(void)
{
  int i;
  float mx;
  float* maxima; 
  ENTRYPTR e;
  SAMPLEPTR s;

  maxima = (float*) xmalloc(nsamples * sizeof(float));

  for (i = 0; i < nsamples; i++) {
    maxima[ i ] = (float)0.0;
  }   

  for (i = 0; i < nidents; i++) {
    e = identtable[ i ];
    for (s = e->samples; s; s = s->next) {
      maxima[ s->n ] += s->nbytes;
    }    
  } 

  for (mx = maxima[ 0 ], i = 0; i < nsamples; i++) {
    if (maxima[ i ] > mx) mx = maxima[ i ];
  } 
        
  free(maxima);
  
  return (mx);
}

/*
 *      Calculate the width of the key.
 */
static double KeyWidth(void)
{
  int i;
  double c;

  c = 0;

  for (i = 0; i < nidents; i++)
    c = maxDouble(c, StringSize(identtable[i]->name));

  c += 3 * borderspace;

  c += KEY_BOX_WIDTH;

  return c;
}

static void Dimensions(void)
{
    xrange = sampletable[ nsamples - 1 ]-sampletable[0]; /*16/11/1995, Niels maaske her der skal korrigeres for sampletable[0]*/
 
    if (fixedYRange >= 0) {
      yrange = fixedYRange;
    }
    else {
      if (showMax) 
	yrange = maxDouble(maxValue, MaxCombinedHeight()); /* If the stack is large then maxValue<MaxCombinedHeight. */
      else
	yrange = MaxCombinedHeight();
    }

    graphwidth  = titlewidth - graphx0 - KeyWidth();
    graphheight = borderheight - titleheight - (2 * borderspace) - graphy0;

    #if DEBUG_DIMENSIONS
    printf("xrange: %5.2f, yrange: %5.2f, graphwidth: %5.2f and graphheight: %5.2f \n", xrange, yrange, graphwidth, graphheight);
    printf("MaxCombinedHeight: %5.2f\n", MaxCombinedHeight());
    printf("maxValue: %5.2f\n", maxValue);
    #endif
    
    return;
}

/***************************************************************************
 * We have to scale all y-values w.r.t. graphheight, before printing.      *
 ***************************************************************************/

/*
 * Scale() -- Scale the values from the samples so that they will fit on the 
 *            page.     
 */
static void Scale(void)
{
  int i;
  float sf;
  ENTRYPTR e;
  SAMPLEPTR s;
  

  #if CHAT
  printf("Scale -- ENTER\n");
  #endif

  sf = (float)(graphheight / yrange);
  for (i = 0; i < nidents; i++) {
    e = identtable[ i ];
    for (s = e->samples; s; s = s->next) {
      s->nbytes *= sf;
    }
  }

  #if DEBUG_SCALE
  printf("sf: %8.6f\n", sf);
  printf("MaxCombinedHeight: %8.4f\n", MaxCombinedHeight());
  #endif

  #if CHAT
  printf("Scale -- LEAVE\n");
  #endif

  return;
}

/***************************************************************************/
/* profiling functions.                                                    */
/***************************************************************************/
void GraphReset()
{
  sampletable = (float *) xmalloc(SampleMax * sizeof(float));
  jobstring = MallocString("Jobstring");
  datestring = MallocString("Datestring");
  programname = MallocString("Programname");
  output = &PsOutput;

  return;
}

void PutFile()
{

  MakeIdentTable();
  SortIdentTable(); /* Sort identifiers by size. */
  TopBands();       /* Keep only noOfBands bands and put rest in an other group. */
  Dimensions(); 
  Scale();

  #if PRINT_IDENT_TABLE
    printIdentTable();
  #endif  

  /* Start printing graph. */
  Prologue();
  output->Fonts();
  BorderOutlineBox();

  TitleOutlineBox();
  TitleText();
  
  CurvesInit();
  
  Key();
  
  Curves();

  Axes();

  if (showMax)
    drawMaxValue(maxValue, maxValueStr);
  if (mflag) Marks();
  if (cflag) Comments();
  output->Prologue();

  return;
}





