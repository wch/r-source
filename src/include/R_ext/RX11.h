#ifndef R_X11_MODULE_H
#define R_X11_MODULE_H

#include <R_ext/Boolean.h>

/*
 We have problems here in that the X11 and relevant R header files 
 may not have been included at this point.
 So to give the full and complete declarations so that they are visible
 to the X11.c file in src/unix and the module in src/modules/X11/devX11.c
 requires that we have these header files.

 We can chose to provide only partial declarations and avoid referring
 to data types that are not yet defined here.  For example, we could have

  typedef  Rboolean (*R_X11DeviceDriverRoutine)();
  typedef  SEXP     (*R_X11DataEntryRoutine)();
  typedef  Rboolean (*R_GetX11ImageRoutine)();

 However, then we risk having mismatched routines in the two locations
 We can even have minimal type information by defining the routine
 pointers simply as DL_FUNC (taken from R_ext/Rdynload.h)

 For the moment, we use the full declarations and assume the X11.h and Xlib.h
 files are available. Perhaps this should also be contained within a
 conditional block determined by whether HAVE_X11 is defined. Is RX11.h
 used when X11 is not available?
 
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
					     int maxcube, 
					     int canvascolor);

typedef SEXP (*R_X11DataEntryRoutine)(SEXP call, SEXP op, SEXP args, SEXP rho);
typedef Rboolean (*R_GetX11ImageRoutine)(int d, XImage **pximage, int *pwidth, int *pheight);
#else

#include <R_ext/Rdynload.h>

typedef DL_FUNC R_X11DeviceDriverRoutine;
typedef DL_FUNC R_X11DataEntryRoutine;
typedef DL_FUNC R_GetX11ImageRoutine;

#endif



void R_setX11Routines(R_X11DeviceDriverRoutine dev, R_X11DataEntryRoutine dataEntry, R_GetX11ImageRoutine image);


#endif /* R_X11_MODULE_H */
