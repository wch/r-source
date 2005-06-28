#include <R_ext/RStartup.h>

void fpu_setup(Rboolean);	/* ./sys-unix.c */

void Rstd_read_history(char *s);

void Rstd_Suicide(char *s);
void Rstd_ShowMessage(char *s);
int  Rstd_ReadConsole(char *prompt, unsigned char *buf, int len, 
		      int addtohistory);
void Rstd_WriteConsole(char *buf, int len);
void Rstd_ResetConsole(void);
void Rstd_FlushConsole(void);
void Rstd_ClearerrConsole(void);
void Rstd_Busy(int which);
void Rstd_CleanUp(SA_TYPE saveact, int status, int runLast);
int  Rstd_ShowFiles(int nfile, char **file, char **headers, char *wtitle,
		    Rboolean del, char *pager);
int  Rstd_ChooseFile(int new, char *buf, int len);
void Rstd_loadhistory(SEXP call, SEXP op, SEXP args, SEXP env);
void Rstd_savehistory(SEXP call, SEXP op, SEXP args, SEXP env);

void R_load_X11_shlib(void);
