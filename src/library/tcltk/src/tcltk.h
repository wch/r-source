#include <stdio.h>
#include <string.h>
#include <tcl.h>
#include <tk.h>

#if (TCL_MAJOR_VERSION==8 && TCL_MINOR_VERSION==0)
#define TCL80
#endif

/* TclCmdProc was redefined to include const in Tcl 8.4 */
#ifndef CONST84
#define CONST84
#endif

#include <Rinternals.h>
#include <R_ext/PrtUtil.h>
#ifndef Win32
#include <R_ext/eventloop.h>
#endif

#include <R_ext/Parse.h>

/* Globals exported from  ./tcltk.c : */

void tcltk_init(void);

SEXP dotTcl(SEXP args);
SEXP dotTclObjv(SEXP args);
SEXP dotTclcallback(SEXP args);

/* Used by .C */

#ifdef Win32
void tcltk_start(void);
void tcltk_end(void);
#else
void delTcl(void);
void RTcl_ActivateConsole(void);
#endif

/* Used by .Extern */

SEXP RTcl_ObjFromVar(SEXP args);
SEXP RTcl_AssignObjToVar(SEXP args);
SEXP RTcl_StringFromObj(SEXP args);
SEXP RTcl_ObjAsCharVector(SEXP args);
SEXP RTcl_ObjAsDoubleVector(SEXP args);
SEXP RTcl_ObjAsIntVector(SEXP args);
SEXP RTcl_ObjFromCharVector(SEXP args);
SEXP RTcl_ObjFromDoubleVector(SEXP args);
SEXP RTcl_ObjFromIntVector(SEXP args);
SEXP RTcl_GetArrayElem(SEXP args);
SEXP RTcl_SetArrayElem(SEXP args);
SEXP RTcl_RemoveArrayElem(SEXP args);
