/* Globals exported from  ./tcltk.c : */

void tcltk_init(void);

SEXP dotTcl(SEXP args);
SEXP dotTclcallback(SEXP args);

#ifndef Win32
void TclHandler(void);
void addTcl(void);
void delTcl(void);
#endif

