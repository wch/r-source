#include <R_ext/RStartup.h>

void fpu_setup(Rboolean);	/* ./sys-unix.c */

void Rstd_read_history(const char *s);

void Rstd_Suicide(const char *s);
void Rstd_ShowMessage(const char *s);
int  Rstd_ReadConsole(const char *prompt, unsigned char *buf, int len,
		      int addtohistory);
void Rstd_WriteConsole(const char *buf, int len);
void Rstd_WriteConsoleEx(const char *buf, int len, int otype);
void Rstd_ResetConsole(void);
void Rstd_FlushConsole(void);
void Rstd_ClearerrConsole(void);
void Rstd_Busy(int which);
void Rstd_CleanUp(SA_TYPE saveact, int status, int runLast);
int  Rstd_ShowFiles(int nfile, const char **file, const char **headers,
		    const char *wtitle, Rboolean del, const char *pager);
int  Rstd_ChooseFile(int _new, char *buf, int len);
void Rstd_loadhistory(SEXP call, SEXP op, SEXP args, SEXP env);
void Rstd_savehistory(SEXP call, SEXP op, SEXP args, SEXP env);
void Rstd_addhistory(SEXP call, SEXP op, SEXP args, SEXP env);

void R_load_X11_shlib(void);
