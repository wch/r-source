#ifndef CONNECTIONS_H
#define CONNECTIONS_H


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

extern InputHandler *InputHandlers;

#endif
