/* Declarations (for devX11.c) which are not only there,
 * ------------ but also, e.g., in  system.c  [event loop]
 */
int X11ConnectionNumber();
void ProcessEvents(void);
int X11DeviceDriver(DevDesc*, char*, double, double, double);
