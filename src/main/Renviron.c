/*
 *   R : A Computer Language for Statistical Data Analysis
 *   Copyright (C) 1997-2023   The R Core Team
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

/* <UTF8> This does byte-level access, e.g. isspace, but is OK. */

/* ------------------- process .Renviron files in C -----------------
 *  Formerly part of ../unix/sys-common.c.
 */

#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h> // for size_t
#endif

/* RENVIRON_WIN32_STANDALONE is set when compiling for use in Rcmd Windows
   front-end, which is not linked against the R library. The locale is not
   initialized in the front-end.

   When RENVIRON_WIN32_STANDALONE is not set, the code below can be used
   both when R is starting up (to read standard Renviron files) as well
   as when R is already running (e.g. to read custom Renviron files).
   In the former case, the locale is not yet initialized and R warnings/errors
   can not be emitted, yet.

   When the locale is not initialized, the MBCS-aware functions below will 
   behave as MBCS-unaware, because mbcslocale will not be set. The reading of
   Renviron when R is starting up (on all platforms) would hence have issues
   with some characters in non-UTF-8 MBCS locale (when bytes can be confused
   for ASCII). Error messages may be imperfectly truncated also in UTF-8. 
*/
#ifdef RENVIRON_WIN32_STANDALONE

/* #undef HAVE_SETENV */
# define HAVE_PUTENV 1

# define Renviron_strchr strchr
# define Renviron_snprintf snprintf

static void Renviron_warning(const char *msg)
{
    fprintf(stderr, "%s\n", msg);
}

static void Renviron_error(const char *msg)
{
    fprintf(stderr, "FATAL ERROR:%s\n", msg);
    exit(2);
}

#else /* not RENVIRON_WIN32_STANDALONE */

# ifdef HAVE_CONFIG_H
#  include <config.h>
# endif
# include <Defn.h>
# include <Fileio.h>
# include <Rinterface.h>

# ifdef Win32
#  include <trioremap.h> /* to ensure snprintf result is null terminated */
# endif

# define Renviron_strchr Rf_strchr
# define Renviron_snprintf Rsnprintf_mbcs

static void Renviron_warning(const char *msg)
{
    if (R_Is_Running > 1)
	warningcall(R_NilValue, "%s", msg);
    else
	R_ShowMessage(msg);
}

static void Renviron_error(const char *msg)
{
    if (R_Is_Running > 1)
	errorcall(R_NilValue, "%s", msg);
    else
	R_Suicide(msg);
}

#endif

static void *Renviron_malloc(size_t size)
{
    void *buf = malloc(size);
    if(!buf) Renviron_error("allocation failure in reading Renviron");
    return buf;
}


/* remove leading and trailing space */
static char *rmspace(char *s)
{
    ssize_t i; // to be safe

    for (i = strlen(s) - 1; i >= 0 && isspace((int)s[i]); i--) s[i] = '\0';
    for (i = 0; isspace((int)s[i]); i++);
    return s + i;
}

/* look for ${FOO-bar} or ${FOO:-bar} constructs, recursively.
   return "" on an error condition.
 */

static char *subterm(char *s)
{
    char *p, *q;
    int colon = 0;

    if(strncmp(s, "${", 2)) return s;
    if(s[strlen(s) - 1] != '}') return s;
    /*  remove leading ${ and final } */
    s[strlen(s) - 1] = '\0';
    s += 2;
    s = rmspace(s);
    if(!strlen(s)) return "";
    p = Renviron_strchr(s, '-');
    if(p) {
	q = p + 1; /* start of value */
	if(p - s > 1 && *(p-1) == ':') {
	    colon = 1;
	    *(p-1) = '\0';
	} else *p = '\0';
    } else q = NULL;
    p = getenv(s);
    if(colon) {
	if(p && strlen(p)) return p; /* variable was set and non-empty */
    } else {
	if(p) return p; /* variable was set */
    }
    return q ? subterm(q) : (char *) "";
}

/* skip along until we find an unmatched right brace */
static char *findRbrace(char *s)
{
    char *p = s, *pl, *pr;
    int nl = 0, nr = 0;

    while(nr <= nl) {
	pl = Renviron_strchr(p, '{');
	pr = Renviron_strchr(p, '}');
	if(!pr) return NULL;
	if(!pl || pr < pl) {
	    p = pr+1; nr++;
	} else {
	    p = pl+1; nl++;
	}
    }
    return pr;
}

#define BUF_SIZE 100000
static char *findterm(char *s)
{
    char *p, *q, *r2, *ss=s;
    static char ans[BUF_SIZE];

    if(!strlen(s)) return "";
    ans[0] = '\0';
    while(1) {
	/* Look for ${...}, taking care to look for inner matches */
	p = Renviron_strchr(s, '$');
	if(!p || p[1] != '{') break;
	q = findRbrace(p+2);
	if(!q) break;
	/* copy over leading part */
	size_t nans = strlen(ans);
	strncat(ans, s, (size_t) (p - s)); ans[nans + p - s] = '\0';
	char r[q - p + 2];
	strncpy(r, p, (size_t) (q - p + 1));
	r[q - p + 1] = '\0';
	r2 = subterm(r);
	if(strlen(ans) + strlen(r2) < BUF_SIZE) strcat(ans, r2); else return ss;
	/* now repeat on the tail */
	s = q+1;
    }
    if(strlen(ans) + strlen(s) < BUF_SIZE) strcat(ans, s); else return ss;
    return ans;
}

static void Putenv(char *a, char *b)
{
    char *buf, *value, *p, *q, quote='\0';
    int inquote = 0;
    int failed = 0;

#ifdef HAVE_SETENV
    buf = (char *) Renviron_malloc((strlen(b) + 1) * sizeof(char));
    value = buf;
#else
    buf = (char *) Renviron_malloc((strlen(a) + strlen(b) + 2) * sizeof(char));
    strcpy(buf, a); strcat(buf, "=");
    value = buf+strlen(buf);
#endif

    /* now process the value */
    for(p = b, q = value; *p; p++) {
	/* remove quotes around sections, preserve \ inside quotes */
	if(!inquote && (*p == '"' || *p == '\'') &&
	   (p == b || *(p-1) != '\\')) {

	    inquote = 1;
	    quote = *p;
	    continue;
	}
	if(inquote && *p == quote && *(p-1) != '\\') {
	    inquote = 0;
	    continue;
	}
	if(!inquote && *p == '\\') {
	    if(*(p+1) == '\n') p++;
	    else if(*(p+1) == '\\') *q++ = *p;
	    continue;
	}
	if(inquote && *p == '\\' && *(p+1) == quote) continue;
	*q++ = *p;
    }
    *q = '\0';
#ifdef HAVE_SETENV
    if(setenv(a, buf, 1))
	failed = 1;
    free(buf);
#elif defined(HAVE_PUTENV)
    if(putenv(buf))
	failed = 1;
    /* no free here: storage remains in use */
#else
    /* pretty pointless, and was not tested prior to 2.3.0 */
    free(buf);
#endif
    if (failed) {
	char buf[1024];
	Renviron_snprintf(buf, 1024,
#ifdef RENVIRON_WIN32_STANDALONE
	                  "Problem in setting variable '%s' in Renviron",
#else
	                  _("problem in setting variable '%s' in Renviron"),
#endif
	                  a);
	Renviron_warning(buf);
    }
}


#define MSG_SIZE 2048

#ifdef RENVIRON_WIN32_STANDALONE
int process_Renviron(const char *filename)
#else
static int process_Renviron(const char *filename)
#endif
{
    FILE *fp;

    if (!filename) return 0;
#ifdef RENVIRON_WIN32_STANDALONE
    fp = fopen(filename, "rt");
#else
    fp = R_fopen(filename, "r");
#endif
    if (!fp) return 0;

    char sm[BUF_SIZE], msg[MSG_SIZE];
    const char *line_prefix = "\n      ";
    const char *ignored_msg = "\n   They were ignored\n";
    const char *truncated_msg = "[... truncated]";
    const char *too_long = " (too long)";
    int errs = 0;

    while(fgets(sm, BUF_SIZE, fp)) {
	sm[BUF_SIZE-1] = '\0'; /* should not be needed */
	/* embedded nulls are not supported */
	int complete_line = feof(fp) || Renviron_strchr(sm, '\n');
	char *s = rmspace(sm), *p;
	if(strlen(s) == 0 || s[0] == '#') continue;
	if(!(p = Renviron_strchr(s, '=')) || !complete_line) {
	    if(!errs) {
		errs = 1;
		Renviron_snprintf(msg, MSG_SIZE,
			       "\n   File %s contains invalid line(s)",
		               filename);
	    }
	    if (strlen(msg) + strlen(line_prefix) + strlen(s) +
	        strlen(ignored_msg) < MSG_SIZE) {

		strcat(msg, line_prefix);
		strcat(msg, s);
	    } else if (strlen(msg) + strlen(line_prefix) +
		                45 + strlen(truncated_msg) +
                                     strlen(ignored_msg) < MSG_SIZE) {
		strcat(msg, line_prefix);
		strncat(msg, s, 45);
#ifndef RENVIRON_WIN32_STANDALONE
		mbcsTruncateToValid(msg);
#endif
		strcat(msg, truncated_msg);
	    }
	    if (!complete_line) {
		if (strlen(msg) + strlen(too_long) +
		    strlen(ignored_msg) < MSG_SIZE) {

		    strcat(msg, too_long);
		}
		/* skip the rest of the line */
		while(!complete_line && fgets(sm, BUF_SIZE, fp)) {
		    sm[BUF_SIZE-1] = '\0'; /* should not be needed */
		    complete_line = feof(fp) || Renviron_strchr(sm, '\n');
		}
		if (!complete_line)
		    break; /* error or EOF at line start */
	    }
	    continue;
	}
	*p = '\0';
	char* lhs = rmspace(s),
	    * rhs = findterm(rmspace(p+1));
	/* set lhs = rhs */
	if(strlen(lhs) && strlen(rhs)) Putenv(lhs, rhs);
    }
    fclose(fp);
    if (errs) {
	if (strlen(msg) + strlen(ignored_msg) < MSG_SIZE)
	   strcat(msg, ignored_msg);
	Renviron_warning(msg);
    }
    return 1;
}

#ifndef RENVIRON_WIN32_STANDALONE

/* try system Renviron: R_HOME/etc/Renviron.  Unix only. */
void process_system_Renviron(void)
{
    char *buf;
    int res;
    size_t needed =  strlen(R_Home) + strlen("/etc/Renviron") + 1;

#ifdef R_ARCH
    needed += strlen(R_ARCH) + 1;
#endif

#ifdef Unix
    if(needed > R_PATH_MAX) {
	Renviron_warning("path to system Renviron is too long: skipping");
	return;
    }
#endif
    buf = (char *) Renviron_malloc(needed);

#ifdef R_ARCH
    strcpy(buf, R_Home);
    strcat(buf, "/etc/");
    strcat(buf, R_ARCH);
    strcat(buf, "/Renviron");
#else
    strcpy(buf, R_Home);
    strcat(buf, "/etc/Renviron");
#endif
    res = process_Renviron(buf);
    free(buf);
    if (!res)
	Renviron_warning("cannot find system Renviron");
}

#ifdef HAVE_UNISTD_H
#include <unistd.h> /* for access, R_OK */
#endif

/* try site Renviron: R_ENVIRON, then R_HOME/etc/Renviron.site. */
void process_site_Renviron (void)
{
    char *buf, *p = getenv("R_ENVIRON");
    size_t needed;

    if(p) {
	if(*p) process_Renviron(p);
	return;
    }
#ifdef R_ARCH
    needed = strlen(R_Home) + strlen("/etc/Renviron.site") + 2 + strlen(R_ARCH);
    int skip = 0;
# ifdef Unix
    if(needed > R_PATH_MAX) {
	Renviron_warning("path to arch-specific Renviron.site is too long: skipping");
	skip = 1;
    } 
# endif
    if(!skip) {
	buf = (char *) Renviron_malloc(needed);
	snprintf(buf, needed, "%s/etc/%s/Renviron.site", R_Home, R_ARCH);
	if(access(buf, R_OK) == 0) {
	    process_Renviron(buf);
	    free(buf);
	    return;
	}
	free(buf);
    }
#endif

    needed = strlen(R_Home) + strlen("/etc/Renviron.site") + 1;
# ifdef Unix
    if(needed > R_PATH_MAX) {
	Renviron_warning("path to Renviron.site is too long: skipping");
	return;
    }
# endif
    buf = (char *) Renviron_malloc(needed);
    snprintf(buf, needed, "%s/etc/Renviron.site", R_Home);
    process_Renviron(buf);
    free(buf);
}

#ifdef Win32
extern char *getRUser(void);
extern void freeRUser(char *);
#endif

static void process_arch_specific_user_Renviron(const char *s)
{
#ifdef R_ARCH
    size_t needed = strlen(s) + 1 + strlen(R_ARCH) + 1;
    int skip = 0;
# ifdef Unix
    if (needed > R_PATH_MAX) {
	Renviron_warning("path to arch-specific user Renviron is too long: skipping");
	skip = 1;
    }
# endif
    if (!skip) {
	char *buf = (char *) Renviron_malloc(needed);
	snprintf(buf, needed, "%s.%s", s, R_ARCH);
	int res = process_Renviron(buf);
	free(buf);
	if (res) return;
    }
#endif
    process_Renviron(s);
}

/* try user Renviron: ./.Renviron, then ~/.Renviron */
void process_user_Renviron(void)
{
    const char *s = getenv("R_ENVIRON_USER");
    size_t needed;
    int skip, res;
    char *buf;

    if(s) {
	if (*s) process_Renviron(R_ExpandFileName(s));
	return;
    }

#ifdef R_ARCH
    needed = strlen(".Renviron.") + strlen(R_ARCH) + 1;
    skip = 0;
# ifdef Unix
    if(needed > R_PATH_MAX) {
	Renviron_warning("path to user Renviron is too long: skipping");
	skip = 1;
    }
# endif
    if (!skip) {
	buf = (char *) Renviron_malloc(needed);
	snprintf(buf, needed, ".Renviron.%s", R_ARCH);
	res = process_Renviron(buf);
	free(buf);
	if (res) return;
    }
#endif

    if(process_Renviron(".Renviron")) return;

#ifdef Unix
    process_arch_specific_user_Renviron(R_ExpandFileName("~/.Renviron"));
#endif
#ifdef Win32
    /* R_USER is not necessarily set yet, so we have to work harder */
    char *RUser = getRUser();
    needed = strlen(RUser) + strlen("/.Renviron") + 1;
    buf = (char *) Renviron_malloc(needed);
    snprintf(buf, needed, "%s/.Renviron", RUser);
    process_arch_specific_user_Renviron(buf);
    freeRUser(RUser);
    free(buf);
#endif
}

attribute_hidden SEXP do_readEnviron(SEXP call, SEXP op, SEXP args, SEXP env)
{

    checkArity(op, args);
    SEXP x = CAR(args);
    if (!isString(x) || LENGTH(x) != 1)
	error(_("argument '%s' must be a character string"), "x");
    const char *fn = R_ExpandFileName(translateChar(STRING_ELT(x, 0)));
    int res = process_Renviron(fn);
    if (!res)
	warning(_("file '%s' cannot be opened for reading"), fn);
    return ScalarLogical(res != 0);
}

#endif /* ^^^ not RENVIRON_WIN32_STANDALONE */
