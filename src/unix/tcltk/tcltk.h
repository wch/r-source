/* Declarations (for tcltk.c) which are not only there,
 * ------------ but also, e.g., in  system.c  [event loop]
 */

#ifdef __SYSTEM__
#define extern
#endif

extern void (*tcltk_ReadConsole)();
extern void (*tcltk_init)();
extern char * (*tk_eval)(char *);
