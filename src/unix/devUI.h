/* Declarations of device  and UI pointers */

#ifdef __SYSTEM__
#define extern
#endif

#include "X11/devX11.h"
#include "Startup.h" /* for SA_TYPE */

extern Rboolean (*ptr_X11DeviceDriver)(DevDesc*, char*, 
				       double, double, double, double,
				       X_COLORTYPE, int);
extern Rboolean (*ptr_R_GetX11Image)(int, void *, int *, int *);
extern Rboolean (*ptr_GnomeDeviceDriver)(DevDesc*, char*, double, double, double);

extern Rboolean (*ptr_GTKDeviceDriver)(DevDesc*, char*, double, double, double);

extern SEXP (*ptr_dataentry)(SEXP call, SEXP op, SEXP args, SEXP rho);

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


#ifdef __SYSTEM__
#undef extern
#endif

Rboolean stub_X11DeviceDriver(DevDesc*, char*, double, double, double, double, 
			      X_COLORTYPE, int);
Rboolean stub_R_GetX11Image(int, void *, int *, int *);
Rboolean stub_GnomeDeviceDriver(DevDesc*, char*, double, double, double);

Rboolean stub_GTKDeviceDriver(DevDesc*, char*, double, double, double);

SEXP stub_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho);

