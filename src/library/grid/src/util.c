#include "grid.h"

/* Get the list element named str, or return NULL. 
 * Copied from the Writing R Extensions manual (which copied it from nls) 
 */
SEXP getListElement(SEXP list, char *str)
{
  SEXP elmt = R_NilValue;
  SEXP names = getAttrib(list, R_NamesSymbol);
  int i;

  for (i = 0; i < length(list); i++)
    if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
      elmt = VECTOR_ELT(list, i);
      break;
    }
  return elmt;
}

void setListElement(SEXP list, char *str, SEXP value) 
{
  SEXP names = getAttrib(list, R_NamesSymbol);
  int i;

  for (i = 0; i < length(list); i++)
    if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
      SET_VECTOR_ELT(list, i, value);
      break;
    }
}
    
/* The lattice R code checks values to make sure that they are numeric
 * BUT we do not know whether the values are integer or real
 * SO we have to be careful when extracting numeric values.
 * This function assumes that x is numeric (obviously).
 */
double numeric(SEXP x, int index)
{
    if (isReal(x))
	return REAL(x)[index];
    else if (isInteger(x))
	return INTEGER(x)[index];
    return NA_REAL;
}

/***********************
 * Stuff for rectangles
 ***********************/

/* Fill a rectangle struct
 */
void rect(double x1, double x2, double x3, double x4, 
	  double y1, double y2, double y3, double y4, 
	  LRect *r)
{
    r->x1 = x1;
    r->x2 = x2;
    r->x3 = x3;
    r->x4 = x4;
    r->y1 = y1;
    r->y2 = y2;
    r->y3 = y3;
    r->y4 = y4;
}

void copyRect(LRect r1, LRect *r) 
{
    r->x1 = r1.x1;
    r->x2 = r1.x2;
    r->x3 = r1.x3;
    r->x4 = r1.x4;
    r->y1 = r1.y1;
    r->y2 = r1.y2;
    r->y3 = r1.y3;
    r->y4 = r1.y4;
}

/* Do two lines intersect ? 
 * Algorithm from Paul Bourke 
 * (http://www.swin.edu.au/astronomy/pbourke/geometry/lineline2d/index.html)
 */
int linesIntersect(double x1, double x2, double x3, double x4,
		   double y1, double y2, double y3, double y4)
{
    double result = 0;
    double denom = (y4 - y3)*(x2 - x1) - (x4 - x3)*(y2 - y1);
    double ua = ((x4 - x3)*(y1 - y3) - (y4 - y3)*(x1 - x3));
    /* If the lines are parallel ...
     */
    if (denom == 0) {
	/* If the lines are coincident ...
	 */
	if (ua == 0) { 
	    /* If the lines are vertical ...
	     */
	    if (x1 == x2) {
		/* Compare y-values
		 */
		if (!((y1 < y3 && fmax2(y1, y2) < fmin2(y3, y4)) ||
		      (y3 < y1 && fmax2(y3, y4) < fmin2(y1, y2))))
		    result = 1;
	    } else {
		/* Compare x-values
		 */
		if (!((x1 < x3 && fmax2(x1, x2) < fmin2(x3, x4)) ||
		      (x3 < x1 && fmax2(x3, x4) < fmin2(x1, x2))))
		    result = 1;
	    }
	}
    }
    /* ... otherwise, calculate where the lines intersect ...
     */
    else {
	double ub = ((x2 - x1)*(y1 - y3) - (y2 - y1)*(x1 - x3));
	ua = ua/denom;
	ub = ub/denom; 
	/* Check for overlap 
	 */
	if ((ua > 0 && ua < 1) && (ub > 0 && ub < 1)) 
	    result = 1;
    }
    return result;
}

int edgesIntersect(double x1, double x2, double y1, double y2,
		   LRect r)
{
    int result = 0;
    if (linesIntersect(x1, x2, r.x1, r.x2, y1, y2, r.y1, r.y2) ||
	linesIntersect(x1, x2, r.x2, r.x3, y1, y2, r.y2, r.y3) ||
	linesIntersect(x1, x2, r.x3, r.x4, y1, y2, r.y3, r.y4) ||
	linesIntersect(x1, x2, r.x4, r.x1, y1, y2, r.y4, r.y1))
	result = 1;
    return result;
}

/* Do two rects intersect ?
 * For each edge in r1, does the edge intersect with any edge in r2 ?
 * FIXME:  Should add first check for non-intersection of 
 * bounding boxes of rects (?)
 */
int intersect(LRect r1, LRect r2) 
{
    int result = 0;
    if (edgesIntersect(r1.x1, r1.x2, r1.y1, r1.y2, r2) ||
	edgesIntersect(r1.x2, r1.x3, r1.y2, r1.y3, r2) ||
	edgesIntersect(r1.x3, r1.x4, r1.y3, r1.y4, r2) ||
	edgesIntersect(r1.x4, r1.x1, r1.y4, r1.y1, r2))
	result = 1;
    return result;
}

/* FIXME:  Nicked this from Graphics.h
 * Should export it instead.
 */
#define      DEG2RAD 0.01745329251994329576

/* Calculate the bounding rectangle for a string.
 * x and y assumed to be in INCHES.
 */
void textRect(double x, double y, SEXP text, int i,
	      R_GE_gcontext *gc,
	      double xadj, double yadj,
	      double rot, GEDevDesc *dd, LRect *r) 
{
    /* NOTE that we must work in inches for the angles to be correct
     */
    LLocation bl, br, tr, tl;
    LLocation tbl, tbr, ttr, ttl;
    LTransform thisLocation, thisRotation, thisJustification;
    LTransform tempTransform, transform;
    double w, h;
    if (isExpression(text)) {
	SEXP expr = VECTOR_ELT(text, i % LENGTH(text));
	w = fromDeviceWidth(GEExpressionWidth(expr, gc, dd),
			    GE_INCHES, dd);
	h = fromDeviceHeight(GEExpressionHeight(expr, gc, dd),
			     GE_INCHES, dd);
    } else {
	char* string = translateChar(STRING_ELT(text, i % LENGTH(text)));
	w = fromDeviceWidth(GEStrWidth(string, gc, dd),
			    GE_INCHES, dd);
	h = fromDeviceHeight(GEStrHeight(string, gc, dd),
			     GE_INCHES, dd);
    }
    location(0, 0, bl);
    location(w, 0, br);
    location(w, h, tr);
    location(0, h, tl);
    translation(-xadj*w, -yadj*h, thisJustification);
    translation(x, y, thisLocation);
    if (rot != 0)
	rotation(rot, thisRotation);
    else
	identity(thisRotation);
    /* Position relative to origin of rotation THEN rotate.
     */
    multiply(thisJustification, thisRotation, tempTransform);
    /* Translate to (x, y)
     */
    multiply(tempTransform, thisLocation, transform);
    trans(bl, transform, tbl);
    trans(br, transform, tbr);
    trans(tr, transform, ttr);
    trans(tl, transform, ttl);
    rect(locationX(tbl), locationX(tbr), locationX(ttr), locationX(ttl),
	 locationY(tbl), locationY(tbr), locationY(ttr), locationY(ttl),
	 r);
    /* For debugging, the following prints out an R statement to draw the
     * bounding box
     */
    /*
    Rprintf("\ngrid.lines(c(%f, %f, %f, %f, %f), c(%f, %f, %f, %f, %f), default.units=\"inches\")\n",
	locationX(tbl), locationX(tbr), locationX(ttr), locationX(ttl),
	 locationX(tbl),
	 locationY(tbl), locationY(tbr), locationY(ttr), locationY(ttl),
	 locationY(tbl)
	 ); 
    */
}
        
/***********************
 * Stuff for making persistent graphical objects
 ***********************/

/* Will have already created an SEXP in R.  This just stores the
 * SEXP in an external reference so that I can get multiple
 * references to it.
 */
SEXP L_CreateSEXPPtr(SEXP s)
{
    /* Allocate a list of length one on the R heap
     */
    SEXP data, result;
    PROTECT(data = allocVector(VECSXP, 1));
    SET_VECTOR_ELT(data, 0, s);
    result = R_MakeExternalPtr(data, R_NilValue, data);
    UNPROTECT(1);
    return result;
}

SEXP L_GetSEXPPtr(SEXP sp)
{
    SEXP data = R_ExternalPtrAddr(sp); 
    /* Check for NULL ptr
     * This can occur if, for example, a grid grob is saved
     * and then loaded.  The saved grob has its ptr null'ed
     */
    if (data == NULL)
	error("grid grob object is empty");
    return VECTOR_ELT(data, 0);
}

SEXP L_SetSEXPPtr(SEXP sp, SEXP s)
{
    SEXP data = R_ExternalPtrAddr(sp);
    /* Check for NULL ptr
     * This can occur if, for example, a grid grob is saved
     * and then loaded.  The saved grob has its ptr null'ed
     */
    if (data == NULL)
	error("grid grob object is empty");
    SET_VECTOR_ELT(data, 0, s);
    return R_NilValue;
}

