/* Declarations (for devX11.c) which are not only there,
 * ------------ but also, e.g., in  system.c  [event loop]
 */

int (*X11ConnectionNumber)();
void (*pR_ProcessEvents)(void);
int (*X11DeviceDriver)(DevDesc*, char*, double, double, double, double, int, int);
SEXP (*ptr_dataentry)(SEXP call, SEXP op, SEXP args, SEXP rho);

#ifdef SOON
int XFigDeviceDriver(char**, int, double*, int);
#endif

/* do nothing function for console version */
int GnomeDeviceDriver(DevDesc*, char*, double, double, double);

int stub_X11ConnectionNumber();
void stub_R_ProcessEvents(void);
int stub_X11DeviceDriver(DevDesc*, char*, double, double, double, double, int, int);
SEXP stub_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho);


