#include "Startup.h"

void fpu_setup(Rboolean);/* ./sys-unix.c */

void R_ShowMessage(char *s);
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

void Rgnome_Suicide(char *s);
void Rgnome_ShowMessage(char *s);
int  Rgnome_ReadConsole(char *prompt, unsigned char *buf, int len, 
			int addtohistory);
void Rgnome_WriteConsole(char *buf, int len);
void Rgnome_ResetConsole(void);
void Rgnome_FlushConsole(void);
void Rgnome_ClearerrConsole(void);
void Rgnome_Busy(int which);
void Rgnome_CleanUp(SA_TYPE saveact, int status, int runLast);
int  Rgnome_ShowFiles(int nfile, char **file, char **headers, char *wtitle,
		      Rboolean del, char *pager);
int  Rgnome_ChooseFile(int new, char *buf, int len);

void Raqua_StartConsole(Rboolean OpenConsole);
int  Raqua_ReadConsole(char *prompt, unsigned char *buf, int len, 
			int addtohistory);
void Raqua_WriteConsole(char *buf, int len);
void Raqua_ResetConsole(void);
void Raqua_FlushConsole(void);
void Raqua_ClearerrConsole(void);


void R_load_X11_shlib(void);
void R_load_gnome_shlib(void);
void R_load_aqua_shlib(void);

int Rf_initEmbeddedR(int argc, char **argv);
