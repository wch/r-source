/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2006-2023  The R Core Team
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
 *  https://www.R-project.org/Licenses/
 */

/* This is intended to be used in scripts like

#! /path/to/Rscript --vanilla
commandArgs(TRUE)
q(status=7)

This invokes R with a command line like
R --no-echo --no-restore --vanilla --file=foo [script_args]

*/

/* execv exists and can be used on Windows, but it returns immediately
   and so the exit status is lost.  HAVE_EXECV is defined under Windows.

   The main reason for using execv rather than system is to avoid
   argument quoting hell.
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#ifdef _WIN32
#include <psignal.h>
/* on some systems needs to be included before <sys/types.h> */
#endif

#include <stdio.h>
#include <limits.h> /* for PATH_MAX */
#include <string.h>
#include <stdlib.h>
#include <unistd.h> /* for execv */

#include <Rversion.h>

/* See comments in Defn.h and keep in step. */
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

#ifdef Unix
# define R_PATH_MAX PATH_MAX
#else
  /* On Windows, 260 is too limiting */
# define R_PATH_MAX 5000
#endif

#ifndef _WIN32
#ifndef R_ARCH /* R_ARCH should be always defined, but for safety ... */
#define R_ARCH ""
#endif

static char rhome[] = R_HOME;
static char rarch[] = R_ARCH;
#else
# ifndef BINDIR
#  define BINDIR "bin"
# endif
# define FOR_Rscript
# include "rterm.c"
#endif

#ifdef HAVE_EXECV
static int verbose = 0;
#endif

void R_putenv_cpy(char *varname, char *value)
{
#ifdef HAVE_PUTENV
    size_t needed = strlen(varname) + 1 + strlen(value) + 1;
    char *buf = (char *)malloc(needed);
    if (!buf) {
	fprintf(stderr, "malloc failure\n");
	exit(1);
    }
    snprintf(buf, needed, "%s=%s", varname, value);
    if (putenv(buf)) {
	fprintf(stderr, "unable to set %s\n", varname);
	exit(1);
    }
    /* no free here: storage remains in use */
#else
    fprintf(stderr, "unable to set %s\n", varname);
    exit(1);
#endif
}

void usage(void)
{
    fprintf(stdout, "Usage: Rscript [options] file [args]\n");
    fprintf(stdout, "   or: Rscript [options] -e expr [-e expr2 ...] [args]\n");
    fprintf(stdout, "A binary front-end to R, for use in scripting applications.\n\n");
    fprintf(stdout, "Options:\n");
    fprintf(stdout, "  --help              Print usage and exit\n");
    fprintf(stdout, "  --version           Print version and exit\n");
    fprintf(stdout, "  --verbose           Print information on progress\n");
    fprintf(stdout, "  --default-packages=LIST  Attach these packages on startup;\n");
    fprintf(stdout, "                        a comma-separated LIST of package names, or 'NULL'\n");
    fprintf(stdout, "and options to R (in addition to --no-echo --no-restore), for example:\n");
    fprintf(stdout, "  --save              Do save workspace at the end of the session\n");
    fprintf(stdout, "  --no-environ        Don't read the site and user environment files\n");
    fprintf(stdout, "  --no-site-file      Don't read the site-wide Rprofile\n");
    fprintf(stdout, "  --no-init-file      Don't read the user R profile\n");
    fprintf(stdout, "  --restore           Do restore previously saved objects at startup\n");
    fprintf(stdout, "  --vanilla           Combine --no-save, --no-restore, --no-site-file,\n");
    fprintf(stdout, "                        --no-init-file and --no-environ\n");
    /* fprintf(stdout, "\n'file' may contain spaces but not shell metacharacters.\n"); */
    fprintf(stdout, "\nExpressions (one or more '-e <expr>') may be used *instead* of 'file'.\n");
    fprintf(stdout, "Any additional 'args' can be accessed from R via 'commandArgs(TRUE)'.\n");
    fprintf(stdout, "See also  ?Rscript  from within R.\n");
}


int main(int argc_, char *argv_[])
{
#ifdef HAVE_EXECV
    char *cmd = NULL, buf[R_PATH_MAX+8], *p;
    int i, i0 = 0, ac = 0, res = 0, e_mode = 0, set_dp = 0;
    char **av;
    int have_cmdarg_default_packages = 0;

    if(argc_ <= 1) {
	usage();
	exit(1);
    }

    /* When executed via '#!' on most systems, argv_[1] will include multiple
       arguments. These arguments will be those provided directly on the line
       starting with '#!'.

       argv_[1] is split here into individual arguments assuming any space or
       tab is a separator - no quoting is supported

       This code is, however, also used with explicit invocation of Rscript
       where arguments are not joined and the first argument may be a file
       name, which is explicitly allowed to contain space. Thus, only split the
       first argument if it starts with "--"  (a file name for Rscript cannot
       start with "--"; it can start with "-", but the only short option is "-e"
       and that is not usable with '#!' invocation).
    */

    /* compute number of arguments included in argv_[1] */
    char *s = argv_[1];
    int njoined = 0;
    size_t j;
    if (strncmp(s, "--", 2) == 0)
	for(j = 0; s[j] != 0; j++)
	    if (s[j] != ' ' && s[j] != '\t' &&
		    (j == 0 || s[j-1] == ' ' || s[j-1] == '\t'))
		/* first character of an argument */
		njoined++;

    int argc;
    char **argv;

    if (njoined > 1) { /* need to split argv_[1] */
	argc = argc_ - 1 + njoined;
	argv = (char **) malloc((size_t) (argc+1)*sizeof(char *));
	if (!argv) {
	    fprintf(stderr, "malloc failure\n");
	    exit(1);
	}
	argv[0] = argv_[0];

	size_t len = strlen(s);
	char *buf = (char *)malloc((size_t) (len+1)*sizeof(char));
	if (!buf) {
	    fprintf(stderr, "malloc failure\n");
	    exit(1);
	}
	strcpy(buf, s);

	i = 1;
	for(j = 0; s[j] != 0; j++)
	    if (s[j] == ' ' || s[j] == '\t')
		/* turn space into end-of-string */
		buf[j] = 0;
	    else if (j == 0 || s[j-1] == ' ' || s[j-1] == '\t')
		/* first character of an argument */
		argv[i++] = buf + j;
	/* assert i - 1 == njoined */

	for(i = 2; i < argc_; i++)
	    argv[i-1+njoined] = argv_[i];
	argv[argc] = 0;

    } else {
	argc = argc_;
	argv = argv_;
    }

    av = (char **) malloc((size_t) (argc+4)*sizeof(char *));
    if(!av) {
	fprintf(stderr, "malloc failure\n");
	exit(1);
    }

    p = getenv("RHOME");
#ifdef _WIN32
    size_t rterm_len = strlen("\\Rterm.exe");
    if(p && *p) {
	size_t len = strlen(p) + 1 + strlen(BINDIR) + rterm_len + 1;
	cmd = (char *)malloc(len);
	if (!cmd) {
	    fprintf(stderr, "malloc failure\n");
	    exit(1);
	}
	snprintf(cmd, len, "%s\\%s\\Rterm.exe",  p, BINDIR);
    } else {
	DWORD size = 1;
	/* GetModuleFileName doesn't return the needed buffer size. */
	for(;;) {
	    cmd = (char *)malloc(size + rterm_len);
	    if (!cmd) {
		fprintf(stderr, "malloc failure\n");
		exit(1);
	    }
	    DWORD res = GetModuleFileName(NULL, cmd, size);
	    if (res > 0 && res < size) /* success */
		break;
	    free(cmd);
	    cmd = NULL;
	    if (res != size) { /* error */
		fprintf(stderr, "installation problem\n");
		exit(1);
	    }
	    size *= 2; /* try again with 2x larger buffer */
	}

	p = strrchr(cmd,'\\');
	if(!p) {fprintf(stderr, "installation problem\n"); exit(1);}
	*p = '\0';
	strcat(cmd, "\\Rterm.exe");
    }
#else
    cmd = (char *)malloc(R_PATH_MAX + 1);
    if (!cmd) {
	fprintf(stderr, "malloc failure\n");
	exit(1);
    }
    if(!(p && *p)) p = rhome;
    /* avoid snprintf here */
    if(strlen(p) + 6 > R_PATH_MAX) {
	fprintf(stderr, "impossibly long path for RHOME\n");
	exit(1);
    }
    snprintf(cmd, R_PATH_MAX+1, "%s/bin/R", p);
#endif
    av[ac++] = cmd;
    av[ac++] = "--no-echo";
    av[ac++] = "--no-restore";

    if(argc == 2) {
	if(strcmp(argv[1], "--help") == 0) {
	    usage();
	    exit(0);
	}
	if(strcmp(argv[1], "--version") == 0) {
	    if(strlen(R_STATUS) == 0)
		fprintf(stdout, "Rscript (R) version %s.%s (%s-%s-%s)\n",
			R_MAJOR, R_MINOR, R_YEAR, R_MONTH, R_DAY);
	    else
		fprintf(stdout, "Rscript (R) version %s.%s %s (%s-%s-%s r%d)\n",
			R_MAJOR, R_MINOR, R_STATUS, R_YEAR, R_MONTH, R_DAY,
			R_SVN_REVISION);
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
	if (e_mode) break;
	    /* Once in e_mode, only additional -e options are to be processed.
	       Any remaining --options belong to the expressions (PR#18102). */ 
	if(strncmp(argv[i], "--", 2) != 0) break;
	if(strcmp(argv[i], "--verbose") == 0) {
	    verbose = 1;
	    i0 = i;
	    continue;
	}
	if(strncmp(argv[i], "--default-packages=", 18) == 0) {
	    set_dp = 1;
	    R_putenv_cpy("R_DEFAULT_PACKAGES", argv[i]+19);
	    if(verbose)
		fprintf(stderr, "setting '%s=%s'\n", "R_DEFAULT_PACKAGES", argv[i]+19);
	    have_cmdarg_default_packages = 1;
	    i0 = i;
	    continue;
	}
	av[ac++] = argv[i];
	i0 = i;
    }

    if(!e_mode) {
	if(++i0 >= argc) {
	    fprintf(stderr, "file name is missing\n");
	    exit(1);
	}
	if(strlen(argv[i0]) > R_PATH_MAX) {
	    fprintf(stderr, "file name is too long\n");
	    exit(1);
	}
	snprintf(buf, R_PATH_MAX+8, "--file=%s", argv[i0]);
	av[ac++] = buf;
    }
    // copy any user arguments, preceded by "--args"
    i = i0+1;
    if (i < argc) {
	av[ac++] = "--args";
	for(; i < argc; i++)
	    av[ac++] = argv[i];
    }
    av[ac] = (char *) NULL;
    /* If provided, and default packages are not specified on the
       command line, then R_SCRIPT_DEFAULT_PACKAGES takes precedence
       over R_DEFAULT_PACKAGES. */
    if (! have_cmdarg_default_packages) {
	char *rsdp = getenv("R_SCRIPT_DEFAULT_PACKAGES");
	if (rsdp)
	    R_putenv_cpy("R_DEFAULT_PACKAGES", rsdp);
    }

    p = getenv("R_SCRIPT_LEGACY");
    int legacy = (p && (strcmp(p, "yes") == 0)) ? 1 : 0;
    //int legacy = (p && (strcmp(p, "no") == 0)) ? 0 : 1;
    if(legacy && !set_dp && !getenv("R_DEFAULT_PACKAGES"))
	/* R_putenv_cpy to get error handling */
	R_putenv_cpy("R_DEFAULT_PACKAGES",
	             "datasets,utils,grDevices,graphics,stats");

#ifndef _WIN32
    /* pass on r_arch from this binary to R as a default */
    if (!getenv("R_ARCH") && *rarch) {
	char *slrarch = (char *)malloc(1 + strlen(rarch) + 1);
	if (!slrarch) {
	    fprintf(stderr, "malloc failure\n");
	    exit(1);
	}
	strcpy(slrarch, "/");
	strcat(slrarch, rarch);
	R_putenv_cpy("R_ARCH", slrarch);
	free(slrarch);
    }
#endif
    if(verbose) {
	fprintf(stderr, "running\n  '%s", cmd);
	for(i = 1; i < ac; i++) fprintf(stderr, " %s", av[i]);
	fprintf(stderr, "'\n\n");
    }
#ifndef _WIN32
    res = execv(cmd, av); /* will not return if R is launched */
    perror("Rscript execution error");
#else
    AppMain(ac, av);
#endif
    return res;
#else /* No execv*/
    fprintf(stderr, "Rscript is not supported on this system");
    exit(1);
#endif
}

