#ifndef R_EXT_EVENTLOOP_H
#define R_EXT_EVENTLOOP_H

#ifdef  __cplusplus
extern "C" {
#endif

#define XActivity 1
#define StdinActivity 2

typedef void (*InputHandlerProc)(void *userData); 

typedef struct _InputHandler {

  int activity;
  int fileDescriptor;
  InputHandlerProc handler;

  struct _InputHandler *next;

    /* Whether we should be listening to this file descriptor or not. */
  int active;

    /* Data that can be passed to the routine as its only argument.
       This might be a user-level function or closure when we implement
       a callback to R mechanism. 
     */
  void *userData;

} InputHandler;


extern InputHandler *initStdinHandler(void);
extern void consoleInputHandler(unsigned char *buf, int len);

extern InputHandler *addInputHandler(InputHandler *handlers, int fd, InputHandlerProc handler, int activity);
extern InputHandler *getInputHandler(InputHandler *handlers, int fd);
extern int           removeInputHandler(InputHandler **handlers, InputHandler *it);
extern InputHandler *getSelectedHandler(InputHandler *handlers, fd_set *mask);

#ifdef __SYSTEM__
InputHandler *R_InputHandlers;
#else
extern InputHandler *R_InputHandlers;
#endif

extern void (* R_PolledEvents)(void);
extern int R_wait_usec;

#ifdef  __cplusplus
}
#endif

#endif /* R_EXT_EVENTLOOP_H */
