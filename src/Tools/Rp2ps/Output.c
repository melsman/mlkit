#include "Output.h"
#include "Rp2Ps.h"

/*------------------------------------*
 * Global declarations.               *
 *------------------------------------*/
double borderheight   = BORDER_HEIGHT;
double borderwidth    = BORDER_WIDTH;
double borderspace    = BORDER_SPACE;
/*double borderradius   = BORDER_RADIUS;*/

double titlewidth     = (BORDER_WIDTH  - (2 * BORDER_SPACE)); 
double titleheight    = TITLE_HEIGHT;
double titleradius    = TITLE_RADIUS;
double titletextspace = TITLE_TEXT_SPACE;

double linethick = LINE_THICK;

double graphx0 = GRAPH_X0;
double graphy0 = GRAPH_Y0;

double areabelow;

Format *output;                  /* Pointer to the chosen format functions.    */

/***************************************************************
 * Font size table.                                            *
 ***************************************************************/
/*
 *      A desperately grim solution.
 */
int fonttab[] = {
    /*  20 (' ') = */ 3,
    /*  21 ('!') = */ 1,
    /*  22 ('"') = */ 1,
    /*  23 ('#') = */ 3,
    /*  24 ('$') = */ 3,
    /*  25 ('%') = */ 3,
    /*  26 ('&') = */ 3,
    /*  27 (''') = */ 1,
    /*  28 ('(') = */ 3,
    /*  29 (')') = */ 3,
    /*  2a ('*') = */ 2,
    /*  2b ('+') = */ 3,
    /*  2c (',') = */ 1,
    /*  2d ('-') = */ 3,
    /*  2e ('.') = */ 1,
    /*  2f ('/') = */ 3,
    /*  30 ('0') = */ 4,
    /*  31 ('1') = */ 4,
    /*  32 ('2') = */ 4,
    /*  33 ('3') = */ 4,
    /*  34 ('4') = */ 4,
    /*  35 ('5') = */ 4,
    /*  36 ('6') = */ 4,
    /*  37 ('7') = */ 4,
    /*  38 ('8') = */ 4,
    /*  39 ('9') = */ 4,
    /*  3a (':') = */ 1,
    /*  3b (';') = */ 1,
    /*  3c ('<') = */ 3,
    /*  3d ('=') = */ 3,
    /*  3e ('>') = */ 3,
    /*  3f ('?') = */ 2,
    /*  40 ('@') = */ 3,
    /*  41 ('A') = */ 5,
    /*  42 ('B') = */ 5,
    /*  43 ('C') = */ 5,
    /*  44 ('D') = */ 5,
    /*  45 ('E') = */ 5,
    /*  46 ('F') = */ 5,
    /*  47 ('G') = */ 5,
    /*  48 ('H') = */ 5,
    /*  49 ('I') = */ 1,
    /*  4a ('J') = */ 5,
    /*  4b ('K') = */ 5,
    /*  4c ('L') = */ 5,
    /*  4d ('M') = */ 5,
    /*  4e ('N') = */ 5,
    /*  4f ('O') = */ 5,
    /*  50 ('P') = */ 5,
    /*  51 ('Q') = */ 5,
    /*  52 ('R') = */ 5,
    /*  53 ('S') = */ 5,
    /*  54 ('T') = */ 5,
    /*  55 ('U') = */ 5,
    /*  56 ('V') = */ 5,
    /*  57 ('W') = */ 5,
    /*  58 ('X') = */ 5,
    /*  59 ('Y') = */ 5,
    /*  5a ('Z') = */ 5,
    /*  5b ('[') = */ 2,
    /*  5c ('\') = */ 3,
    /*  5d (']') = */ 2,
    /*  5e ('^') = */ 1,
    /*  5f ('_') = */ 3,
    /*  60 ('`') = */ 1,
    /*  61 ('a') = */ 3,
    /*  62 ('b') = */ 3,
    /*  63 ('c') = */ 3,
    /*  64 ('d') = */ 3,
    /*  65 ('e') = */ 3,
    /*  66 ('f') = */ 3,
    /*  67 ('g') = */ 3,
    /*  68 ('h') = */ 3,
    /*  69 ('i') = */ 1,
    /*  6a ('j') = */ 2,
    /*  6b ('k') = */ 3,
    /*  6c ('l') = */ 1,
    /*  6d ('m') = */ 5,
    /*  6e ('n') = */ 3,
    /*  6f ('o') = */ 3,
    /*  70 ('p') = */ 3,
    /*  71 ('q') = */ 3,
    /*  72 ('r') = */ 2,
    /*  73 ('s') = */ 3,
    /*  74 ('t') = */ 2,
    /*  75 ('u') = */ 3,
    /*  76 ('v') = */ 3,
    /*  77 ('w') = */ 3,
    /*  78 ('x') = */ 3,
    /*  79 ('y') = */ 3,
    /*  7a ('z') = */ 3,
    /*  7b ('{') = */ 2,
    /*  7c ('|') = */ 1,
    /*  7d ('}') = */ 2,
    /*  7e ('~') = */ 2
};

double StringSize(char *s)
{
  double r;

  for (r = 0; *s; s++)
    if(*s>=0x20 && *s <= 0x7e)
      r += fonttab[(*s) - 0x20];

  return (FUDGE * r);
}

/***************************************************************************
 * Functions for producing the graph. The variable output points at the    *
 * functions used to produce the output, that is ps (PsOutput),            *
 * mif (MifOutput) or draw (DrawOutput) format.                            *
 ***************************************************************************/
void Prologue(void)
{
  double scale;
  /*if (eflag) scale = epsfwidth / (double) borderwidth; */
  scale = epsfwidth / (double) borderwidth;

  /*printf("eflag: %d, gflag: %d, scale: %8.2f\n", eflag, gflag, scale);*/

  output->Prelude(scale,eflag);

  if (eflag) {
      output->Scale(scale);
  } else if (gflag) {
      output->Portrait();
  } else {
      output->Landscape();
  }
}

void outputBox(int fill,double x,double y,double w,double h)
{
  output->PathNew(Closed,4);
  if (fill>= 0)
    output->PathFill(fill);
  output->PathWidth(0.2);
  output->PathMoveTo(x,y);
  output->PathLine(0.0, h);
  output->PathLine(w, 0.0);
  output->PathLine(0.0, -h);
  output->PathStroke();
}

void BorderOutlineBox(void)
{
  outputBox(-1,0.0,0.0,borderwidth,borderheight);
}

void TitleOutlineBox(void)
{
  outputBox(-1,borderspace,borderheight - titleheight - borderspace
	    ,titlewidth,titleheight);
}

void TitleText(void)
{
  double x, y;
  char info[100];

  x = borderspace + titletextspace;
  y = borderheight - titleheight - borderspace + titletextspace;

  sprintf(info,"%d %s x %s",(int) areabelow,yLab,"seconds"); /*ylab was bytes17/11/1995, Niels*/

    /* job identifier goes at far left */
  output->Text(JustifyLeft,x,y,TITLE_FONT,jobstring);
    /* area below curve is centered */
  /*output->Text(JustifyCenter,titlewidth/2,y,TITLE_FONT,info);*/ /*niels remove top center text. */
  output->Text(JustifyRight,titlewidth,y,TITLE_FONT,datestring);
}

void outputLine(double x,double y,double dx,double dy)
{
  output->PathNew(Open,2);
  output->PathWidth(0.3);
  output->PathMoveTo(x, y);
  output->PathLine(dx, dy);
  output->PathStroke();
}

void outputLineTo(double x,double y,double x2,double y2)
{
  output->PathNew(Open,2);
  output->PathWidth(0.1);
  output->PathMoveTo(x, y);
  output->PathLineTo(x2,y2);
  output->PathStroke();
}

void outputCaret(double x,double y,double d)
{
    output->PathNew(Closed,3);
    output->PathFill(22);
    output->PathWidth(0.5);
    output->PathMoveTo(x - d, y);
    output->PathLine(d, -d);
    output->PathLine(d,  d);
    output->PathStroke();
}

int FontSize(int font)
{
  switch(font) {
  case TITLE_FONT:    return LARGE_FONT;
  case SCALE_FONT:    return NORMAL_FONT;
  case KEY_FONT:      return NORMAL_FONT;
  case SMALLKEY_FONT: return SMALL_FONT;
  }
  return NORMAL_FONT;
}
