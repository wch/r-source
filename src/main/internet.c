/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-4   The R Core Team.
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

attribute_hidden
void Rsockopen(int *port)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockopen)(port);
    else
	error(_("socket routines cannot be loaded"));
}

attribute_hidden
void Rsocklisten(int *sockp, char **buf, int *len)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->socklisten)(sockp, buf, len);
    else
	error(_("socket routines cannot be loaded"));
}

attribute_hidden
void Rsockconnect(int *port, char **host)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockconnect)(port, host);
    else
	error(_("socket routines cannot be loaded"));
}

attribute_hidden
void Rsockclose(int *sockp)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockclose)(sockp);
    else
	error(_("socket routines cannot be loaded"));
}

attribute_hidden
void Rsockread(int *sockp, char **buf, int *maxlen)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockread)(sockp, buf, maxlen);
    else
	error(_("socket routines cannot be loaded"));
}

attribute_hidden
void Rsockwrite(int *sockp, char **buf, int *start, int *end, int *len)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockwrite)(sockp, buf, start, end, len);
    else
	error(_("socket routines cannot be loaded"));
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
