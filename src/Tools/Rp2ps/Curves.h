#ifndef _CURVES_H
#define _CURVES_H

extern double graphheight;
extern double graphwidth;

extern double xrange;
extern double yrange;

/* Functions used to draw the curves, identifiers and axes. */
void CurvesInit(void);
void Curves(void);
void Axes(void);
void Key(void);
void drawMaxValue(float value, char *valStr);
void Comments(void);
void Marks(void);
#endif /* _CURVES_H */
