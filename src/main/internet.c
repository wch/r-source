/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001   The R Development Core Team.
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Rconnections.h>
#include <R_ext/R-ftp-http.h>
#include "R_ext/Rdynpriv.h"

typedef int  (*iDL_FUNC)();
typedef SEXP  (*sDL_FUNC)();
typedef Rconnection  (*rDL_FUNC)();

static sDL_FUNC ptr_download;
static rDL_FUNC ptr_newurl, ptr_newsock;
static DL_FUNC ptr_HTTPOpen, ptr_HTTPClose,  ptr_FTPOpen, ptr_FTPClose;
static iDL_FUNC ptr_HTTPRead, ptr_FTPRead;
static DL_FUNC ptr_sockopen, ptr_socklisten, ptr_sockconnect, ptr_sockclose,
    ptr_sockread, ptr_sockwrite;


/*
SEXP do_download(SEXP call, SEXP op, SEXP args, SEXP env);
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

 */

static int initialized = 0;


static void internet_Init(void)
{
    int res = moduleCdynload("internet", 1, 1);
    initialized = -1;
    if(!res) return;
    if(!(ptr_download = 
	 (sDL_FUNC)R_FindSymbol("do_download", "internet", NULL))) return;
    if(!(ptr_newurl = (rDL_FUNC)R_FindSymbol("R_newurl", "internet", NULL))) return;
    if(!(ptr_newsock = 
	 (rDL_FUNC)R_FindSymbol("R_newsock", "internet", NULL))) return;
    if(!(ptr_HTTPOpen = 
	 (DL_FUNC)R_FindSymbol("R_HTTPOpen", "internet", NULL))) return;
    if(!(ptr_HTTPRead = 
	 (iDL_FUNC)R_FindSymbol("R_HTTPRead", "internet", NULL))) return;
    if(!(ptr_HTTPClose = 
	 (DL_FUNC)R_FindSymbol("R_HTTPClose", "internet", NULL))) return;
    if(!(ptr_FTPOpen = 
	 (DL_FUNC)R_FindSymbol("R_FTPOpen", "internet", NULL))) return;
    if(!(ptr_FTPRead = 
	 (iDL_FUNC)R_FindSymbol("R_FTPRead", "internet", NULL))) return;
    if(!(ptr_FTPClose = 
	 (DL_FUNC)R_FindSymbol("R_FTPClose", "internet", NULL))) return;
    if(!(ptr_sockopen = 
	 (DL_FUNC)R_FindSymbol("Rsockopen", "internet", NULL))) return;
    if(!(ptr_socklisten = 
	 (DL_FUNC)R_FindSymbol("Rsocklisten", "internet", NULL))) return;
    if(!(ptr_sockclose = 
	 (DL_FUNC)R_FindSymbol("Rsockclose", "internet", NULL))) return;
    if(!(ptr_sockconnect = 
	 (DL_FUNC)R_FindSymbol("Rsockconnect", "internet", NULL))) return;
    if(!(ptr_sockread = 
	 (DL_FUNC)R_FindSymbol("Rsockread", "internet", NULL))) return;
    if(!(ptr_sockwrite = 
	 (DL_FUNC)R_FindSymbol("Rsockwrite", "internet", NULL))) return;

    initialized = 1;    
    return;
}


SEXP do_download(SEXP call, SEXP op, SEXP args, SEXP env)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr_download)(call, op, args, env);
    else {
	error("internet routines cannot be loaded");
	return R_NilValue;
    }
}

Rconnection R_newurl(char *description, char *mode)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr_newurl)(description, mode);
    else {
	error("internet routines cannot be loaded");
	return (Rconnection)0;
    }    
}

Rconnection R_newsock(char *host, int port, int server, char *mode)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr_newsock)(host, port, server, mode);
    else {
	error("internet routines cannot be loaded");
	return (Rconnection)0;
    }    
}

void *R_HTTPOpen(const char *url)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr_HTTPOpen)(url);
    else {
	error("internet routines cannot be loaded");
	return NULL;
    }
}

int   R_HTTPRead(void *ctx, char *dest, int len)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr_HTTPRead)(ctx, dest, len);
    else {
	error("internet routines cannot be loaded");
	return 0;
    }    
}

void  R_HTTPClose(void *ctx)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr_HTTPClose)(ctx);
    else
	error("internet routines cannot be loaded");
}

void *R_FTPOpen(const char *url)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr_FTPOpen)(url);
    else {
	error("internet routines cannot be loaded");
	return NULL;
    }
}

int   R_FTPRead(void *ctx, char *dest, int len)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr_FTPRead)(ctx, dest, len);
    else {
	error("internet routines cannot be loaded");
	return 0;
    }    
}

void  R_FTPClose(void *ctx)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr_FTPClose)(ctx);
    else
	error("internet routines cannot be loaded");
}

void Rsockopen(int *port)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr_sockopen)(port);
    else
	error("socket routines cannot be loaded");
}

void Rsocklisten(int *sockp, char **buf, int *len)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr_socklisten)(sockp, buf, len);
    else
	error("socket routines cannot be loaded");
}

void Rsockconnect(int *port, char **host)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr_sockconnect)(port, host);
    else
	error("socket routines cannot be loaded");
}

void Rsockclose(int *sockp)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr_sockclose)(sockp);
    else
	error("socket routines cannot be loaded");
}

void Rsockread(int *sockp, char **buf, int *maxlen)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr_sockread)(sockp, buf, maxlen);
    else
	error("socket routines cannot be loaded");
}

void Rsockwrite(int *sockp, char **buf, int *start, int *end, int *len)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr_sockwrite)(sockp, buf, start, end, len);
    else
	error("socket routines cannot be loaded");
}
