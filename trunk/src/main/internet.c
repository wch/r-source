/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-4   The R Development Core Team.
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
 */

/* <UTF8> char here is either ASCII or handled as a whole */

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
SEXP attribute_hidden do_download(SEXP call, SEXP op, SEXP args, SEXP env);
Rconnection R_newurl(char *description, char *mode);
Rconnection R_newsock(char *host, int port, int server, char *mode);


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

Rconnection attribute_hidden R_newurl(char *description, const char * const mode)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->newurl)(description, mode);
    else {
	error(_("internet routines cannot be loaded"));
	return (Rconnection)0;
    }    
}

Rconnection R_newsock(char *host, int port, int server, char *mode)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->newsock)(host, port, server, mode);
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

void Rsockopen(int *port)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockopen)(port);
    else
	error(_("socket routines cannot be loaded"));
}

void Rsocklisten(int *sockp, char **buf, int *len)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->socklisten)(sockp, buf, len);
    else
	error(_("socket routines cannot be loaded"));
}

void Rsockconnect(int *port, char **host)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockconnect)(port, host);
    else
	error(_("socket routines cannot be loaded"));
}

void Rsockclose(int *sockp)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockclose)(sockp);
    else
	error(_("socket routines cannot be loaded"));
}

void Rsockread(int *sockp, char **buf, int *maxlen)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockread)(sockp, buf, maxlen);
    else
	error(_("socket routines cannot be loaded"));
}

void Rsockwrite(int *sockp, char **buf, int *start, int *end, int *len)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockwrite)(sockp, buf, start, end, len);
    else
	error(_("socket routines cannot be loaded"));
}

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
    
