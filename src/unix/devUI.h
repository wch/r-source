/* Declarations of device and UI pointers. */

#ifdef __SYSTEM__
# define extern
#endif

#include "../modules/X11/devX11.h" /* for X_COLORTYPE, but don't get the
				      definition of the struct.  */
#include "Startup.h"		/* for SA_TYPE */

typedef Rboolean (*X11DeviceDriverRoutine)(DevDesc*, char*, 
					   double, double, double, double,
					   X_COLORTYPE, int, int);

extern X11DeviceDriverRoutine ptr_X11DeviceDriver;

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
extern int (*R_timeout_handler)();
extern long R_timeout_val;

extern DevDesc* 
Rf_addX11Device(char *display, double height, double width, double ps, 
		double gamma, int colormodel, int maxcubesize, 
		int canvascolor, char *devname, 
		X11DeviceDriverRoutine deviceDriverRoutine);

#ifdef __SYSTEM__
# undef extern
#endif

extern Rboolean stub_X11DeviceDriver(DevDesc*, char*, double, double,
				     double, double, X_COLORTYPE, int,
				     int);
extern Rboolean stub_R_GetX11Image(int, void *, int *, int *);

Rboolean stub_GnomeDeviceDriver(DevDesc*, char*, double, double, double);
Rboolean stub_GTKDeviceDriver(DevDesc*, char*, double, double, double);

extern SEXP stub_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho);

