/* Declarations of device and UI pointers. */

#ifdef __SYSTEM__
# define extern
#endif

#include "Startup.h"		/* for SA_TYPE */

extern Rboolean (*ptr_GnomeDeviceDriver)(DevDesc*, char*, double, double, double);

extern Rboolean (*ptr_GTKDeviceDriver)(DevDesc*, char*, double, double, double);
#ifdef HAVE_AQUA
extern void (*ptr_R_StartConsole)();
#endif

extern void (*ptr_R_Suicide)(char *);
extern void (*ptr_R_ShowMessage)();
extern int  (*ptr_R_ReadConsole)(char *, unsigned char *, int, int);
extern void (*ptr_R_WriteConsole)(char *, int);
extern void (*ptr_R_ResetConsole)();
extern void (*ptr_R_FlushConsole)();
extern void (*ptr_R_ClearerrConsole)();
extern void (*ptr_R_Busy)(int);
extern void (*ptr_R_CleanUp)(SA_TYPE, int, int);
extern int  (*ptr_R_ShowFiles)(int, char **, char **, char *, Rboolean, char *);
extern int  (*ptr_R_ChooseFile)(int, char *, int);
extern void (*ptr_R_loadhistory)(SEXP, SEXP, SEXP, SEXP);
extern void (*ptr_R_savehistory)(SEXP, SEXP, SEXP, SEXP);
extern int (*R_timeout_handler)();
extern long R_timeout_val;


#ifdef __SYSTEM__
# undef extern
#endif

Rboolean stub_GnomeDeviceDriver(DevDesc*, char*, double, double, double);
Rboolean stub_GTKDeviceDriver(DevDesc*, char*, double, double, double);
