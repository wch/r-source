/* Declarations (for devX11.c) which are not only there,
 * ------------ but also, e.g., in  system.c  [event loop]
 */

int  (*X11DeviceDriver)(DevDesc*, char*, double, double, double, double,
			int, int);
SEXP (*ptr_dataentry)(SEXP call, SEXP op, SEXP args, SEXP rho);

int (*GnomeDeviceDriver)(DevDesc*, char*, double, double, double);

int (*GTKDeviceDriver)(DevDesc*, char*, double, double, double);

int stub_X11DeviceDriver(DevDesc*, char*, double, double, double, double, 
			 int, int);
int stub_GnomeDeviceDriver(DevDesc*, char*, double, double, double);

int stub_GTKDeviceDriver(DevDesc*, char*, double, double, double);

SEXP stub_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho);

void (*ptr_R_Suicide)(char *);
void (*ptr_R_ShowMessage)();
int  (*ptr_R_ReadConsole)(char *, unsigned char *, int, int);
void (*ptr_R_WriteConsole)(char *, int);
void (*ptr_R_ResetConsole)();
void (*ptr_R_FlushConsole)();
void (*ptr_R_ClearerrConsole)();
void (*ptr_R_Busy)(int);
void (*ptr_R_CleanUp)(int, int, int);
int  (*ptr_R_ShowFiles)(int, char **, char **, char *, int, char *);
int  (*ptr_R_ChooseFile)(int, char *, int);
