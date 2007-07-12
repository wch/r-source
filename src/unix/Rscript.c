/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2006-7  The R Development Core Team
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

/* This is intended to be used in scripts like

#! /path/to/Rscript --vanilla
commandArgs(TRUE)
q(status=7)

This invokes R with a command line like
R --slave --no-restore --vanilla --file=foo [script_args]

*/

/* execv exists and can be used on Windows, but it returns immediately
   and so the exit status is lost.  HAVE_EXECV is defined under Windows.

   The main reason for using execv rather than system is to avoid 
   argument quoting hell.
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#if defined(HAVE_GLIBC2)
#include <features.h>
# ifndef __USE_SVID
#  define __USE_SVID             /* so that we get putenv declared */
# endif
#endif



#include <stdio.h>
#include <limits.h> /* for PATH_MAX */
#include <string.h>
#include <stdlib.h>
#include <unistd.h> /* for execv */


/* Maximal length of an entire file name */
#if !defined(PATH_MAX)
# if defined(HAVE_SYS_PARAM_H)
#  include <sys/param.h>
# endif
# if !defined(PATH_MAX)
#  if defined(MAXPATHLEN)
#    define PATH_MAX MAXPATHLEN
#  elif defined(Win32)
#    define PATH_MAX 260
#  else
/* quite possibly unlimited, so we make this large, and test when used */
#    define PATH_MAX 5000
#  endif
# endif
#endif

#ifndef WIN32
static char rhome[] = R_HOME;
#else
# include "rterm.c"
#endif

#define RSVERSION "$Rev$"

#ifdef HAVE_EXECV
static int verbose = 0;
#endif

void usage()
{
    fprintf(stderr, "Usage: /path/to/Rscript [--options] [-e expr] file [args]\n\n");
    fprintf(stderr, "--options accepted are\n");
    fprintf(stderr, "  --help              Print usage and exit\n");
    fprintf(stderr, "  --version           Print version and exit\n");
    fprintf(stderr, "  --verbose           Print information on progress\n");
    fprintf(stderr, "  --default-packages=list\n");
    fprintf(stderr, "                      Where 'list' is a comma-separated set\n");
    fprintf(stderr, "                        of package names, or 'NULL'\n");
    fprintf(stderr, "or options to R, in addition to --slave --no-restore, such as\n");
    fprintf(stderr, "  --save              Do save workspace at the end of the session\n");
    fprintf(stderr, "  --no-environ        Don't read the site and user environment files\n");
    fprintf(stderr, "  --no-site-file      Don't read the site-wide Rprofile\n");
    fprintf(stderr, "  --no-init-file      Don't read the .Rprofile or ~/.Rprofile files\n");
    fprintf(stderr, "  --restore           Do restore previously saved objects at startup\n");
    fprintf(stderr, "  --vanilla           Combine --no-save, --no-restore, --no-site-file\n");
    fprintf(stderr, "                        --no-init-file and --no-environ\n");
}


int main(int argc, char *argv[])
{
#ifdef HAVE_EXECV
    char cmd[PATH_MAX+1], buf[PATH_MAX+8], buf2[1100], *p;
    int i, i0 = 0, ac = 0, res = 0, e_mode = 0, set_dp = 0;
    char **av;

    if(argc <= 1) {
	usage();
	exit(1);
    }
    av = (char **) malloc((argc+4)*sizeof(char *));
    if(!av) {
	fprintf(stderr, "malloc failure\n");
	exit(1);
    }

    p = getenv("RHOME");
#ifdef WIN32
    if(p && strlen(p))
	snprintf(cmd, PATH_MAX+1, "%s\\bin\\Rterm.exe",  p);
    else {
	char rhome[MAX_PATH];
	GetModuleFileName(NULL, rhome, MAX_PATH);
	p = strrchr(rhome,'\\');
	if(!p) {fprintf(stderr, "installation problem\n"); exit(1);}
	*p = '\0';
	snprintf(cmd, PATH_MAX+1, "%s\\Rterm.exe",  rhome);
    }
#else
    if(!(p && strlen(p))) p = rhome;
    /* we cannot assume snprintf here */
    if(strlen(p) + 6 > PATH_MAX) {
	fprintf(stderr, "impossibly long path for RHOME\n");
	exit(1);
    }
    sprintf(cmd, "%s/bin/R", p);
#endif
    av[ac++] = cmd;
    av[ac++] = "--slave";
    av[ac++] = "--no-restore";
    
    if(argc == 2) {
	if(strcmp(argv[1], "--help") == 0) {
	    usage();
	    exit(0);
	}
	if(strcmp(argv[1], "--version") == 0) {
	    char buf[20];
	    strcpy(buf, RSVERSION+6);
	    buf[strlen(buf) - 2] = '\0';
	    fprintf(stderr, "R scripting front-end version %s\n", buf);
	    exit(0);
	}
    }
    
    /* first copy over any -e or --foo args */
    for(i = 1; i < argc; i++) {
	if(strcmp(argv[i], "-e") == 0) {
	    e_mode = 1;
	    av[ac++] = argv[i];
	    if(!argv[++i]) {
		fprintf(stderr, "-e not followed by an expression\n");
		exit(1);
	    }
	    av[ac++] = argv[i];
	    i0 = i;
	    continue;
	}
	if(strncmp(argv[i], "--", 2)) break;
	if(strcmp(argv[i], "--verbose") == 0) {
	    verbose = 1;
	    i0 = i;
	    continue;
	}
	if(strncmp(argv[i], "--default-packages=", 18) == 0) {
	    set_dp = 1;
	    if(strlen(argv[i]) > 1000) {
		fprintf(stderr, "unable to set R_DEFAULT_PACKAGES\n");
		exit(1);
	    }
	    sprintf(buf2, "R_DEFAULT_PACKAGES=%s", argv[i]+19);
	    if(verbose)
		fprintf(stderr, "setting '%s'\n", buf2);
#ifdef HAVE_PUTENV
	    if(putenv(buf2)) 
#endif
	    {
		fprintf(stderr, "unable to set R_DEFAULT_PACKAGES\n");
		exit(1);
	    }
	    i0 = i;
	    continue;
	}
	av[ac++] = argv[i];
	i0 = i;
    }

    if(!e_mode) {
	if(strlen(argv[++i0]) > PATH_MAX) {
	    fprintf(stderr, "file name is too long\n");
	    exit(1);
	}
	sprintf(buf, "--file=%s", argv[i0]); 
	av[ac++] = buf;
    }
    av[ac++] = "--args";
    for(i = i0+1; i < argc; i++) av[ac++] = argv[i];
    av[ac] = (char *) NULL;
#ifdef HAVE_PUTENV
    if(!set_dp && !getenv("R_DEFAULT_PACKAGES"))
	putenv("R_DEFAULT_PACKAGES=datasets,utils,grDevices,graphics,stats");
#endif
    if(verbose) {
	fprintf(stderr, "running\n  '%s", cmd);
	for(i = 1; i < ac-1; i++) fprintf(stderr, " %s", av[i]);
	fprintf(stderr, "'\n\n");
    }
#ifndef WIN32
    res = execv(cmd, av); /* will not return if R is launched */
    perror("Rscript execution error");
#else
    AppMain(ac, av);
#endif
    return res;
#else
    fprintf(stderr, "Rscript is not supported on this system");
    exit(1);
#endif
}
