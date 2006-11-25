#ifndef R_INTERNET_MODULE_H
#define R_INTERNET_MODULE_H


#include <Rinternals.h>


typedef SEXP (*R_DownloadRoutine)(SEXP call, SEXP op, SEXP args, SEXP env);
typedef Rconnection (*R_NewUrlRoutine)(char *description, const char * const mode);
typedef Rconnection (*R_NewSockRoutine)(char *host, int port, int server, char *mode); 

typedef void * (*R_HTTPOpenRoutine)(const char *url, const char *headers, const int cacheOK);
typedef int    (*R_HTTPReadRoutine)(void *ctx, char *dest, int len);
typedef void   (*R_HTTPCloseRoutine)(void *ctx);
	      
typedef void * (*R_FTPOpenRoutine)(const char *url);
typedef int    (*R_FTPReadRoutine)(void *ctx, char *dest, int len);
typedef void   (*R_FTPCloseRoutine)(void *ctx);

typedef void   (*R_SockOpenRoutine)(int *port);
typedef void   (*R_SockListenRoutine)(int *sockp, char **buf, int *len);
typedef void   (*R_SockConnectRoutine)(int *port, char **host);
typedef void   (*R_SockCloseRoutine)(int *sockp);

typedef void   (*R_SockReadRoutine)(int *sockp, char **buf, int *maxlen);
typedef void   (*R_SockWriteRoutine)(int *sockp, char **buf, int *start, int *end, int *len);
typedef int    (*R_SockSelectRoutine)(int nsock, int *insockfd, int *ready, int *write, double timeout);


typedef struct {
    R_DownloadRoutine download;
    R_NewUrlRoutine   newurl;
    R_NewSockRoutine  newsock;

    R_HTTPOpenRoutine  HTTPOpen;
    R_HTTPReadRoutine  HTTPRead;
    R_HTTPCloseRoutine HTTPClose;

    R_FTPOpenRoutine   FTPOpen;
    R_FTPReadRoutine   FTPRead;
    R_FTPCloseRoutine  FTPClose;

    R_SockOpenRoutine     sockopen;
    R_SockListenRoutine   socklisten;
    R_SockConnectRoutine  sockconnect;
    R_SockCloseRoutine    sockclose;

    R_SockReadRoutine     sockread;
    R_SockWriteRoutine    sockwrite;
    R_SockSelectRoutine   sockselect;

} R_InternetRoutines;

R_InternetRoutines *R_setInternetRoutines(R_InternetRoutines *routines);

#endif /* ifndef R_INTERNET_MODULE_H */
