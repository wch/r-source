/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file console.c
 *  Copyright (C) 1998--1999  Guido Masarotto and Brian Ripley
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


/* extra commands for R */
#include <stdio.h>
#include "Defn.h"
#include "Fileio.h"
#include <io.h>
#include <time.h>
#include <windows.h>
#include "graphapp/ga.h"

SEXP  do_sysfile(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP  do_tempfile(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP  do_unlink(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP  do_helpstart(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP  do_helpitem(SEXP call, SEXP op, SEXP args, SEXP env);


static void changename(char *s,int maxlen)
{
    char *p, *q;
    char *l = "+-.";
    int   i;

    for (p = s, i = 0; *p; p++) {
	i += 1;
	if (i > maxlen) {
	    *p = '\0';
	    break;
	}
	for (q = l; *q; q++)
	    if (*q == *p) {
		*p = '_';
		break;
	    }
    }
}

char *to83name(char *fname)
{
    static char shortname[MAX_PATH];
    char  fshortname[MAX_PATH];
    char  drv[MAX_PATH], dir[MAX_PATH], name[MAX_PATH], ext[MAX_PATH];
    char  ddrv[MAX_PATH], ddir[MAX_PATH], dname[MAX_PATH], dext[MAX_PATH];

    _splitpath(fname, drv, dir, name, ext);
    changename(name, 8);
    if (strlen(ext) > 1)
	changename(&ext[1], 3);
    sprintf(shortname, "%s%s", name, ext);
    strcpy(ddir, dir);
    ddir[strlen(ddir) - 1] = '\0';
    for (; strlen(ddir) > 0;) {
	_splitpath(ddir, ddrv, dir, dname, dext);
	changename(dname, 8);
	if (strlen(ext) > 1)
	    changename(&dext[1], 3);
	sprintf(fshortname, "%s%s/%s", dname, dext, shortname);
	strcpy(shortname, fshortname);
	strcpy(ddir, dir);
	ddir[strlen(ddir) - 1] = '\0';
    }
    if (strlen(drv) > 0) {
	sprintf(fshortname, "%s/%s", drv, shortname);
	strcpy(shortname, fshortname);
    } else if ((fname[0] == '/') || (fname[0] == '\\')) {
	sprintf(fshortname, "/%s", shortname);
	strcpy(shortname, fshortname);
    }
    return shortname;
}


/*
SEXP do_sysfile(SEXP call, SEXP op, SEXP args, SEXP env)
{
        SEXP ans;
	SEXP tlist = R_NilValue, tchar;
        char tmp[MAX_PATH], *p;
        int done,handle,i,j,k;
        struct _finddata_t fdd;
        checkArity(op, args);
        if( !isString(CAR(args)) || LENGTH(CAR(args)) != 1 )
                errorcall(call, "invalid file name argument\n");
        strcpy(tmp,CHAR(STRING(CAR(args))[0]));
        if ( (handle = _findfirst(tmp,&fdd)) == -1) {
	   p=to83name(tmp);
           handle = _findfirst(p,&fdd);
	   if (handle != -1) strcpy(tmp,p);
	}
        i = 0;
        if (handle != -1) {
	     PROTECT(tlist);
             for (k=strlen(tmp);k > 0; k--)
	       if ((tmp[k-1]=='/')||(tmp[k-1]=='\\')) break;
	     for(done=0;!done;done=_findnext(handle,&fdd)) {
		if (strcmp(fdd.name,".") && strcmp(fdd.name,"..")){
		  tmp[k]='\0';
		  strcat(tmp,fdd.name);
		  tchar = mkChar(tmp);
		  UNPROTECT(1);
		  PROTECT(tlist = CONS(tchar, tlist));
                  i += 1;
	        }
	     }
	     _findclose(handle);
	     if (!i) UNPROTECT(1);
	}
        if (i) {
	       PROTECT(ans = allocVector(STRSXP, i));
	       for (j = (i - 1); j >= 0; j--) {
		 STRING(ans)[j] = CAR(tlist);
		 tlist = CDR(tlist);
	       }
	       UNPROTECT(2);
	}
        else {
            PROTECT(ans = allocVector(STRSXP,1));
            STRING(ans)[0]=mkChar("");
	    UNPROTECT(1);
	}
        return (ans);
}
*/

SEXP do_tempfile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  ans;
    char *tmp, tm[MAX_PATH], *tn;

    checkArity(op, args);
    PROTECT(ans = allocVector(STRSXP, 1));
    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	errorcall(call, "invalid file name argument\n");
    tn = CHAR(STRING(CAR(args))[0]);
    /* try to get a new file name */
    tmp = getenv("TMP");
    if (!tmp)
	tmp = getenv("TEMP");
    if (!tmp)
	getenv("R_HOME");
    if ((strlen(tmp) + strlen(tn)) <= MAX_PATH - 8) {
	sprintf(tm, "%s\\%sXXXXXX", tmp, tn);
	tn = mktemp(tm);
    } else
	tn = NULL;
    if (tn) {
	STRING(ans)[0] = mkChar(tn);
	free(tn);
    } else
	STRING(ans)[0] = mkChar("");
    UNPROTECT(1);
    return (ans);
}

SEXP do_unlink(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  ans;
    char *tmp;

    checkArity(op, args);
    PROTECT(ans = allocVector(STRSXP, 1));
    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	errorcall(call, "invalid file name argument\n");
    tmp = CHAR(STRING(CAR(args))[0]);
    /* try to unlink file */
    if (!unlink(tmp))
	STRING(ans)[0] = mkChar("0");
    else
	STRING(ans)[0] = mkChar("1");
    UNPROTECT(1);
    return (ans);
}

SEXP do_helpstart(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  ans;
    char *home, buf[MAX_PATH];
    FILE *ff;

    checkArity(op, args);
    home = getenv("RHOME");
    if (home == NULL)
	error("RHOME not set\n");
    sprintf(buf, "%s\\doc\\html\\index.html", home);
    ff = fopen(buf, "r");
    if (!ff) {
	sprintf(buf, "%s\\doc\\html\\index.htm", home);
	ff = fopen(buf, "r");
	if (!ff) {
	    sprintf(buf, "%s\\doc\\html\\index.htm[l] not found\n", home);
	    error(buf);
	}
    }
    fclose(ff);
    ShellExecute(NULL, "open", buf, NULL, home, SW_SHOW);
    PROTECT(ans = allocVector(STRSXP, 1));
    STRING(ans)[0] = mkChar("");
    UNPROTECT(1);
    return (ans);
}

static int nhfiles = 0;
static char *hfiles[50];


SEXP do_helpitem(SEXP call, SEXP op, SEXP args, SEXP env)
{
/*
 * type = 1: launch html file.
 *        2: "topic", 2, Windows help file.
 *        3: notify are finished with the help file.
 */

    char *item, *hfile;
    char *home, buf[MAX_PATH];
    FILE *ff;
    int   type;

    checkArity(op, args);
    if (!isString(CAR(args)))
	errorcall(call, "invalid topic argument\n");
    item = CHAR(STRING(CAR(args))[0]);
    type = asInteger(CADR(args));
    if (type == 1) {
	ff = fopen(item, "r");
	if (!ff) {
	    sprintf(buf, "%s not found\n", item);
	    error(buf);
	}
	fclose(ff);
	home = getenv("RHOME");
	if (home == NULL)
	    error("RHOME not set\n");
	ShellExecute(NULL, "open", item, NULL, home, SW_SHOW);
    } else if (type == 2) {
	if (!isString(CADDR(args)))
	    errorcall(call, "invalid hlpfile argument\n");
	hfile = CHAR(STRING(CADDR(args))[0]);
	if (!WinHelp((HWND) 0, hfile, HELP_KEY, (DWORD) item))
	    warning("WinHelp call failed\n");
	else {
	    if (nhfiles >= 50)
		error("too many .hlp files opened\n");
	    hfiles[nhfiles] = malloc(strlen(hfile) * sizeof(char));
	    strcpy(hfiles[nhfiles++], hfile);
	}
    } else if (type == 3) {
	if (!isString(CADDR(args)))
	    warningcall(call, "invalid hlpfile argument\n");
	hfile = CHAR(STRING(CADDR(args))[0]);
	if (!WinHelp((HWND) 0, hfile, HELP_QUIT, (DWORD) 0))
	    error("WinHelp call failed\n");
    } else
	warning("type not yet implemented\n");
    return R_NilValue;
}

void closeAllHlpFiles()
{
    int   i;

    for (i = nhfiles - 1; i >= 0; i--)
	WinHelp((HWND) 0, hfiles[i], HELP_QUIT, (DWORD) 0);
}


SEXP do_flushconsole(SEXP call, SEXP op, SEXP args, SEXP env)
{
    R_FlushConsole();
    return R_NilValue;
}
