/* Globals exported from  ./tcltk.c : */

void tcltk_init(void);

SEXP dotTcl(SEXP args);
SEXP dotTclcallback(SEXP args);

#ifndef Win32
void TclHandler(void);
void addTcl(void);
void delTcl(void);
#endif

/* Used by .C */

void delTcl(void);
void RTcl_ActivateConsole(void);

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
