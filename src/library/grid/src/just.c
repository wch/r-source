#include "grid.h"

/* Modify a location for the correct justification */

/* These tranformations assume that x and width are in the same units */
/* FIXME:  I don't think we check anywhere that a horizontal justification
 * is not L_BOTTOM or L_TOP (i.e., meaningless).  Ditto for checking
 * vertical justification.
 */
double justifyX(double x, double width, int hjust) {
    double result = 0;
    switch (hjust) {
    case L_LEFT: 
	result = x;
	break;        
    case L_RIGHT: 
	result = x - width;
	break;
    case L_CENTRE: 
    case L_CENTER:
	result = x - width/2;
	break;
    }
    return result;
}

double justifyY(double y, double height, int vjust) {
    double result = 0;
    switch (vjust) {
    case L_BOTTOM:  
	result = y;
	break;
    case L_TOP:  
	result = y - height;
	break;
    case L_CENTRE:
    case L_CENTER:
	result = y - height/2;
	break;
    }
    return result;
}

/* Convert enum justification into 0..1 justification */
double convertJust(int just) {
    double result = 0;
    switch (just) {
    case L_BOTTOM:
    case L_LEFT:
	result = 0;
	break;
    case L_CENTRE:
    case L_CENTER:
	result = .5;
	break;
    case L_TOP:
    case L_RIGHT:
	result = 1;
	break;
    }
    return result;
}

/* Return the amount of justification required 
 */
void justification(double width, double height, int hjust, int vjust,
		   double *hadj, double *vadj)
{
    switch (hjust) {
    case L_LEFT: 
	*hadj = 0;
	break;        
    case L_RIGHT: 
	*hadj = -width;
	break;
    case L_CENTRE: 
    case L_CENTER:
	*hadj = -width/2;
	break;
    }
    switch (vjust) {
    case L_BOTTOM:  
	*vadj = 0;
	break;
    case L_TOP:  
	*vadj = -height;
	break;
    case L_CENTRE:
    case L_CENTER:
	*vadj = -height/2;
	break;
    }
}
