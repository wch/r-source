/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-12   The R Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Rconnections.h>
#include <Rdynpriv.h>
#include <R_ext/R-ftp-http.h>
#include <Rmodules/Rinternet.h>

static R_InternetRoutines routines, *ptr = &routines;


/*
SEXP do_download(SEXP call, SEXP op, SEXP args, SEXP env);
Rconnection R_newurl(char *description, char *mode);
Rconnection R_newsock(char *host, int port, int server, char *mode, int timeout);


Next 6 are for use by libxml, only

void *R_HTTPOpen(const char *url);
int   R_HTTPRead(void *ctx, char *dest, int len);
void  R_HTTPClose(void *ctx);

void *R_FTPOpen(const char *url);
int   R_FTPRead(void *ctx, char *dest, int len);
void  R_FTPClose(void *ctx);

void Rsockopen(int *port)
void Rsocklisten(int *sockp, char **buf, int *len)
void Rsockconnect(int *port, char **host)
void Rsockclose(int *sockp)
void Rsockread(int *sockp, char **buf, int *maxlen)
void Rsockwrite(int *sockp, char **buf, int *start, int *end, int *len)

int Rsockselect(int nsock, int *insockfd, int *ready, int *write,
		double timeout)

int R_HTTPDCreate(const char *ip, int port);
void R_HTTPDStop(void);
 */

static int initialized = 0;

R_InternetRoutines *
R_setInternetRoutines(R_InternetRoutines *routines)
{
    R_InternetRoutines *tmp;
    tmp = ptr;
    ptr = routines;
    return(tmp);
}

#ifdef Win32
extern Rboolean UseInternet2;
#endif

static void internet_Init(void)
{
    int res;
#ifdef Win32
    res = UseInternet2 ? R_moduleCdynload("internet2", 1, 1) :
	R_moduleCdynload("internet", 1, 1);
#else
    res = R_moduleCdynload("internet", 1, 1);
#endif
    initialized = -1;
    if(!res) return;
    if(!ptr->download)
	error(_("internet routines cannot be accessed in module"));
    initialized = 1;
    return;
}


SEXP attribute_hidden do_download(SEXP call, SEXP op, SEXP args, SEXP env)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->download)(call, op, args, env);
    else {
	error(_("internet routines cannot be loaded"));
	return R_NilValue;
    }
}

#ifdef Win32
SEXP attribute_hidden do_setInternet2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int newUseInternet2;
    SEXP newval, retval;
    
    PROTECT(retval = ScalarLogical(UseInternet2));
    
    checkArity(op, args);
    newval = CAR(args);
    if (length(newval) != 1) error(_("bad value"));
    newUseInternet2 = asLogical(newval);
    
    if (newUseInternet2 != NA_LOGICAL) {
    	R_Visible = FALSE;
    	if (newUseInternet2 != UseInternet2) {
    	    if (initialized) warning(_("internet routines were already initialized"));
    	    UseInternet2 = newUseInternet2;
    	    initialized = 0;
    	}
    }
    UNPROTECT(1);
    return retval;
}
#endif

Rconnection attribute_hidden R_newurl(const char *description,
				      const char * const mode)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->newurl)(description, mode);
    else {
	error(_("internet routines cannot be loaded"));
	return (Rconnection)0;
    }
}

Rconnection attribute_hidden
R_newsock(const char *host, int port, int server, const char * const mode,
	  int timeout)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->newsock)(host, port, server, mode, timeout);
    else {
	error(_("internet routines cannot be loaded"));
	return (Rconnection)0;
    }
}

void *R_HTTPOpen(const char *url)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->HTTPOpen)(url, NULL, 0);
    else {
	error(_("internet routines cannot be loaded"));
	return NULL;
    }
}

int   R_HTTPRead(void *ctx, char *dest, int len)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->HTTPRead)(ctx, dest, len);
    else {
	error(_("internet routines cannot be loaded"));
	return 0;
    }
}

void  R_HTTPClose(void *ctx)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->HTTPClose)(ctx);
    else
	error(_("internet routines cannot be loaded"));
}

void *R_FTPOpen(const char *url)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->FTPOpen)(url);
    else {
	error(_("internet routines cannot be loaded"));
	return NULL;
    }
}

int   R_FTPRead(void *ctx, char *dest, int len)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->FTPRead)(ctx, dest, len);
    else {
	error(_("internet routines cannot be loaded"));
	return 0;
    }
}

void  R_FTPClose(void *ctx)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->FTPClose)(ctx);
    else
	error(_("internet routines cannot be loaded"));
}

attribute_hidden
int   R_HTTPDCreate(const char *ip, int port)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->HTTPDCreate)(ip, port);
    else
	error(_("internet routines cannot be loaded"));
    return -1;
}

attribute_hidden
void R_HTTPDStop(void)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->HTTPDStop)();
    else
	error(_("internet routines cannot be loaded"));
}

SEXP attribute_hidden do_startHTTPD(SEXP call, SEXP op, SEXP args, SEXP env) 
{
    const char *ip = 0;
    SEXP sIP, sPort;
    checkArity(op, args);
    sIP = CAR(args);
    sPort = CADR(args);
    if (sIP != R_NilValue && (TYPEOF(sIP) != STRSXP || LENGTH(sIP) != 1))
	error(_("invalid bind address specification"));
    if (sIP != R_NilValue)
	ip = CHAR(STRING_ELT(sIP, 0));
    return ScalarInteger(R_HTTPDCreate(ip, asInteger(sPort)));
}

SEXP attribute_hidden do_stopHTTPD(SEXP call, SEXP op, SEXP args, SEXP env) 
{
    checkArity(op, args);
    R_HTTPDStop();
    return R_NilValue;
}


static SEXP Rsockconnect(SEXP sport, SEXP shost)
{
    if (length(sport) != 1) error("invalid 'socket' argument");
    int port = asInteger(sport);
    char *host[1];
    host[0] = (char *) translateChar(STRING_ELT(shost, 0));
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockconnect)(&port, host);
    else
	error(_("socket routines cannot be loaded"));
    return ScalarInteger(port); // The socket number
}

static SEXP Rsockread(SEXP ssock, SEXP smaxlen)
{
    if (length(ssock) != 1) error("invalid 'socket' argument");
    int sock = asInteger(ssock), maxlen = asInteger(smaxlen);
    char buf[maxlen+1], *abuf[1];
    abuf[0] = buf;
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockread)(&sock, abuf, &maxlen);
    else
	error(_("socket routines cannot be loaded"));
    SEXP ans = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(ans, 0, mkCharLen(buf, maxlen));
    UNPROTECT(1);
    return ans;
		       
}

static SEXP Rsockclose(SEXP ssock)
{
    if (length(ssock) != 1) error("invalid 'socket' argument");
    int sock = asInteger(ssock);
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockclose)(&sock);
    else
	error(_("socket routines cannot be loaded"));
    return ScalarLogical(sock);
}

static SEXP Rsockopen(SEXP sport)
{
    if (length(sport) != 1) error("invalid 'port' argument");
    int port = asInteger(sport);
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockopen)(&port);
    else
	error(_("socket routines cannot be loaded"));
    return ScalarInteger(port); // The socket number
}

static SEXP Rsocklisten(SEXP ssock)
{
    if (length(ssock) != 1) error("invalid 'socket' argument");
    int sock = asInteger(ssock), len = 256;
    char buf[257], *abuf[1];
    abuf[0] = buf;
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->socklisten)(&sock, abuf, &len);
    else
	error(_("socket routines cannot be loaded"));
    SEXP ans = PROTECT(ScalarInteger(sock)); // The socket being listened on
    SEXP host = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(host, 0, mkChar(buf));
    setAttrib(ans, install("host"), host);
    UNPROTECT(2);
    return ans;
}

static SEXP Rsockwrite(SEXP ssock, SEXP sstring)
{
    if (length(ssock) != 1) error("invalid 'socket' argument");
    int sock = asInteger(ssock), start = 0, end, len;
    char *buf = (char *) translateChar(STRING_ELT(sstring, 0)), *abuf[1];
    end = len = strlen(buf);
    abuf[0] = buf;
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockwrite)(&sock, abuf, &start, &end, &len);
    else
	error(_("socket routines cannot be loaded"));
    return ScalarInteger(len);
}


attribute_hidden
int Rsockselect(int nsock, int *insockfd, int *ready, int *write,
		double timeout)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->sockselect)(nsock, insockfd, ready, write, timeout);
    else {
	error(_("socket routines cannot be loaded"));
	return 0;
    }
}

attribute_hidden
SEXP do_sock(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans = R_NilValue; /* -Wall */
    checkArity(op, args);
    switch(PRIMVAL(op)) {
    case 0: ans = Rsockconnect(CAR(args), CADR(args));
	break;
    case 1: ans = Rsockopen(CAR(args));
	break;
    case 2: ans = Rsocklisten(CAR(args));
	break;
    case 3: ans = Rsockclose(CAR(args)); 
	break;
    case 4: ans = Rsockread(CAR(args), CADR(args)); 
	break;
    case 5: ans = Rsockwrite(CAR(args), CADR(args)); 
	break;
    }
    return ans;
}
