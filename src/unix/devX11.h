/* Declarations (for devX11.c) which are not only there,
 * ------------ but also, e.g., in  system.c  [event loop]
 */

int X11ConnectionNumber();
void ProcessEvents(void);
int X11DeviceDriver(DevDesc*, char*, double, double, double, double, int, int);
#ifdef SOON
int XFigDeviceDriver(char**, int, double*, int);
#endif

/* do nothing function for console version */
int GnomeDeviceDriver(DevDesc*, char*, double, double, double);
