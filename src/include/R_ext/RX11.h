#ifndef R_X11_MODULE_H
#define R_X11_MODULE_H

#include <Rinternals.h>

typedef SEXP (*R_do_X11)(SEXP call, SEXP op, SEXP args, SEXP rho);
typedef SEXP (*R_X11DataEntryRoutine)(SEXP call, SEXP op, SEXP args, SEXP rho);
typedef Rboolean (*R_GetX11ImageRoutine)(int d, void *pximage, 
					 int *pwidth, int *pheight);

typedef struct {
    R_do_X11 X11;
    R_X11DataEntryRoutine de;
    R_GetX11ImageRoutine  image;
} R_X11Routines;

R_X11Routines *R_setX11Routines(R_X11Routines *routines);


#endif /* R_X11_MODULE_H */
