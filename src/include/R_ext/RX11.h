#ifndef R_X11_MODULE_H
#define R_X11_MODULE_H

#include "R_ext/Boolean.h"

/*
 We have problems here in that the X11 and relevant R header files 
 may not have been included at this point.

 We can also do weak typing, such as
  typedef  Rboolean (*R_X11DeviceDriverRoutine)();
  typedef  SEXP     (*R_X11DataEntryRoutine)();
  typedef  Rboolean (*R_GetX11ImageRoutine)();

 But we have to redefine ptr_X11DeviceDriver, ptr_dataentry, and ptr_R_GetX11Image.
 So for the moment, we leave the declarations as being simple DL_FUNC's.
 The correct declarations as used in src/modules/X11/devX11.c are given
 next.
 */
#if 1

  /* Header files needed for the different types in these prototypes.
     We skip X_COLORTYPE and make it an int. That is defined in
     src/modules/X11/devX11.h
   */
#include <X11/X.h>
#include <X11/Xlib.h> /* XImage */
#include "Graphics.h"

typedef Rboolean (*R_X11DeviceDriverRoutine)(DevDesc *dd,
					     char *disp_name,
					     double width,
					     double height,
					     double pointsize,
					     double gamma_fac,
					     int colormodel, /* really enum of X_COLORTYPE */
					     int maxcube);

typedef SEXP (*R_X11DataEntryRoutine)(SEXP call, SEXP op, SEXP args, SEXP rho);
typedef Rboolean (*R_GetX11ImageRoutine)(int d, XImage **pximage, int *pwidth, int *pheight);
#else

#include "R_ext/Rdynload.h"

typedef DL_FUNC R_X11DeviceDriverRoutine;
typedef DL_FUNC R_X11DataEntryRoutine;
typedef DL_FUNC R_GetX11ImageRoutine;

#endif



void R_setX11Routines(R_X11DeviceDriverRoutine dev, R_X11DataEntryRoutine dataEntry, R_GetX11ImageRoutine image);


#endif /* R_X11_MODULE_H */
