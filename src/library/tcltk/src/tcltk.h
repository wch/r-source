/* Globals exported from  ./tcltk.c : */

void tcltk_init(void);
char* tk_eval(char *cmd);

SEXP dotTcl(SEXP args);
SEXP dotTclcallback(SEXP args);

#ifndef Win32
void TclHandler(void);
void addTcl(void);
void delTcl(void);
#endif

