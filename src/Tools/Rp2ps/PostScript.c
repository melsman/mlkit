#include "PostScript.h"
#include "Error.h"
#include "Rp2Ps.h"
#include "Output.h"

 /***************************************************************************
 * Functions to provide PostScript output.                                 *
 ***************************************************************************/

/*
 *      Print a string s in width w, escaping characters where necessary.
 */
static void EscapePrint(char* s)
{
  for ( ; *s; s++) {
    if (*s == '(') {                /* escape required */
      fputc('\\', outfp);
    } else if (*s == ')') {
      fputc('\\', outfp);
    }

    fputc(*s, outfp);
  }
}

static void PsScale(double scale)
{
  fprintf(outfp, "%f %f scale\n", scale, scale);
}

static void PsLandscape(void)
{
  fprintf(outfp, "-90 rotate\n");
  fprintf(outfp, "%f %f translate\n", (float)-(borderwidth + START_Y),
	  (float)START_X);
}

static void PsPortrait(void)
{
  fprintf(outfp, "%f %f translate\n", (float) START_X, (float) START_Y);
}

static void StandardSpecialComments(void)
{
  fprintf(outfp, "%%!PS-Adobe-2.0\n");
  fprintf(outfp, "%%%%Title: %s\n", jobstring);
  fprintf(outfp, "%%%%Creator: %s (version %s)\n", programname, VERSION);
  fprintf(outfp, "%%%%CreationDate: %s\n", datestring);
  fprintf(outfp, "%%%%EndComments\n");
}

static void EPSFSpecialComments(double scale)
{
  fprintf(outfp, "%%!PS-Adobe-2.0\n");
  fprintf(outfp, "%%%%Title: %s\n", jobstring);
  fprintf(outfp, "%%%%Creator: %s (version %s)\n", programname, VERSION);
  fprintf(outfp, "%%%%CreationDate: %s\n", datestring);
  fprintf(outfp, "%%%%BoundingBox: 0 0 %d %d\n",
	  (int) (borderwidth  * scale + 0.5),
	  (int) (borderheight * scale + 0.5) );
  fprintf(outfp, "%%%%EndComments\n");
}

static void PsPrelude(double scale,int embedded)
{
  if (embedded) {
    EPSFSpecialComments(scale);
  } else {
    StandardSpecialComments();
  }
}

static void PsFonts(void)
{
  fprintf(outfp, "/HE%d /Helvetica findfont %d scalefont def\n",
	  TITLE_FONT, FontSize(TITLE_FONT));

  fprintf(outfp, "/HE%d /Helvetica findfont %d scalefont def\n",
	  SCALE_FONT, FontSize(SCALE_FONT));

  fprintf(outfp, "/HE%d /Helvetica findfont %d scalefont def\n",
	  KEY_FONT, FontSize(KEY_FONT));
  fprintf(outfp, "/HE%d /Helvetica findfont %d scalefont def\n",
	  SMALLKEY_FONT, FontSize(SMALLKEY_FONT));
}

static void PsText(Justify just,double x,double y,int font,char *str)
{
  fprintf(outfp, "HE%d setfont\n", font);
  switch(just) {
  case JustifyLeft:
    fprintf(outfp, "%f %f moveto\n", x, y);
    fputc('(', outfp);
    EscapePrint(str);
    fprintf(outfp, ")\n");
    fprintf(outfp, "show\n");
    break;
  case JustifyCenter:
    fputc('(', outfp);
    EscapePrint(str);
    fprintf(outfp, ")\n");
    fprintf(outfp, "dup stringwidth pop\n");
    fprintf(outfp, "2 div\n");
    fprintf(outfp, "%f\n", x);
    fprintf(outfp, "exch sub\n");
    fprintf(outfp, "%f moveto\n", y);
    fprintf(outfp, "show\n");
    break;
  case JustifyRight:
    fputc('(', outfp);
    EscapePrint(str);
    fprintf(outfp, ")\n");
    fprintf(outfp, "dup stringwidth pop\n");
    fprintf(outfp, "%f\n", x);
    fprintf(outfp, "exch sub\n");
    fprintf(outfp, "%f moveto\n", y);
    fprintf(outfp, "show\n");
    break;
  case JustifyVertical:
    fprintf(outfp, "gsave\n");
    fputc('(', outfp);
    EscapePrint(str);
    fprintf(outfp, ")\n");
    fprintf(outfp, "dup stringwidth pop\n");
    fprintf(outfp, "%f\n", y);
    fprintf(outfp, "exch sub\n");
    fprintf(outfp, "%f exch\n", x);
    fprintf(outfp, "translate\n");
    fprintf(outfp, "90 rotate\n");
    fprintf(outfp, "0 0 moveto\n");
    fprintf(outfp, "show\n");
    fprintf(outfp, "grestore\n");
  }
}

static void PsPrologue(void)
{
  fprintf(outfp, "showpage\n");
}

static int PathColour;
static double PathWidth;
static int PsKind;

static void PsPathNew(Kind kind,int size)
{
  size = size; /* Ignore size */
  PathColour = -1;
  PathWidth = -1;
  PsKind = kind;
  fprintf(outfp, "newpath\n");
}

static void PsPathMoveTo(double x, double y)
{
  fprintf(outfp, "%f %f moveto\n", x, y);
}

static void PsPathLineTo(double x, double y)
{
  fprintf(outfp, "%f %f lineto\n", x, y);
}

static void PsPathLine(double x, double y)
{
  fprintf(outfp, "%f %f rlineto\n", x, y);
}

static void PsPathFill(int fill)
{
  PathColour = fill;
}

static void PsPathWidth(double width)
{
  PathWidth = width;
}

/*static double colours[] =
{ 0.3, 0.6, 0.9, 0.2, 0.5, 0.8, 0.1, 0.4, 0.7, 0.0, 
  0.3, 0.6, 0.9, 0.2, 0.5, 0.8, 0.1, 0.4, 0.7, 0.0, 
  0.3, 0.6
};*/

static double colours[] =
{ 0.9, 0.2, 0.5, 0.8, 0.1, 0.4, 0.7, 0.0, 0.3, 0.6,
  0.9, 0.2, 0.5, 0.8, 0.1, 0.4, 0.7, 0.0, 0.3, 0.6,
  0.9, 0.2
};

static void PsPathStroke(void)
{
  if(PsKind == Closed)
    fprintf(outfp, "closepath\n");
  
  if(PathWidth >= 0)
    fprintf(outfp, "%f  setlinewidth\n",PathWidth);
  
  if(PathColour>=0) {
    fprintf(outfp, "gsave\n");
    
    fprintf(outfp, "%f setgray\n", colours[PathColour%22]);
    fprintf(outfp, "fill\n");

    fprintf(outfp, "grestore\n");
  }
  fprintf(outfp, "stroke\n");
}

Format PsOutput =
{ ".ps"
  ,PsPrelude
  ,PsPrologue
  ,PsScale
  ,PsLandscape
  ,PsPortrait
  ,PsFonts
  ,PsText

  ,PsPathNew
  ,PsPathMoveTo
  ,PsPathLineTo
  ,PsPathLine
  ,PsPathFill
  ,PsPathWidth
  ,PsPathStroke
};
