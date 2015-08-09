/*
 *   R : A Computer Language for Statistical Data Analysis
 *   Copyright (C) 1997-2015   The R Core Team
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h> /* for setenv or putenv */
#include <Defn.h> /* for PATH_MAX */
#include <Rinterface.h>
#include <Fileio.h>
#include <ctype.h>		/* for isspace */

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

    if(strncmp(s, "${", 2)) return s;
    if(s[strlen(s) - 1] != '}') return s;
    /*  remove leading ${ and final } */
    s[strlen(s) - 1] = '\0';
    s += 2;
    s = rmspace(s);
    if(!strlen(s)) return "";
    p = Rf_strchr(s, '-');
    if(p) {
	q = p + 1; /* start of value */
	if(p - s > 1 && *(p-1) == ':') *(p-1) = '\0'; else *p = '\0';
    } else q = NULL;
    p = getenv(s);
    if(p && strlen(p)) return p; /* variable was set and non-empty */
    return q ? subterm(q) : (char *) "";
}

/* skip along until we find an unmatched right brace */
static char *findRbrace(char *s)
{
    char *p = s, *pl, *pr;
    int nl = 0, nr = 0;

    while(nr <= nl) {
	pl = Rf_strchr(p, '{');
	pr = Rf_strchr(p, '}');
	if(!pr) return NULL;
	if(!pl || pr < pl) {
	    p = pr+1; nr++;
	} else {
	    p = pl+1; nl++;
	}
    }
    return pr;
}

#define BUF_SIZE 10000
static char *findterm(char *s)
{
    char *p, *q, *r2, *ss=s;
    static char ans[BUF_SIZE];

    if(!strlen(s)) return "";
    ans[0] = '\0';
    while(1) {
	/* Look for ${...}, taking care to look for inner matches */
	p = Rf_strchr(s, '$');
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

#ifdef HAVE_SETENV
    buf = (char *) malloc((strlen(b) + 1) * sizeof(char));
    if(!buf) R_Suicide("allocation failure in reading Renviron");
    value = buf;
#else
    buf = (char *) malloc((strlen(a) + strlen(b) + 2) * sizeof(char));
    if(!buf) R_Suicide("allocation failure in reading Renviron");
    strcpy(buf, a); strcat(buf, "=");
    value = buf+strlen(buf);
#endif

    /* now process the value */
    for(p = b, q = value; *p; p++) {
	/* remove quotes around sections, preserve \ inside quotes */
	if(!inquote && (*p == '"' || *p == '\'')) {
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
	warningcall(R_NilValue,
		    _("problem in setting variable '%s' in Renviron"), a);
    free(buf);
#elif defined(HAVE_PUTENV)
    if(putenv(buf))
	warningcall(R_NilValue,
		    _("problem in setting variable '%s' in Renviron"), a);
    /* no free here: storage remains in use */
#else
    /* pretty pointless, and was not tested prior to 2.3.0 */
    free(buf);
#endif
}


#define MSG_SIZE 2000
static int process_Renviron(const char *filename)
{
    FILE *fp;
    char *s, *p, sm[BUF_SIZE], *lhs, *rhs, msg[MSG_SIZE+50];
    int errs = 0;

    if (!filename || !(fp = R_fopen(filename, "r"))) return 0;
    snprintf(msg, MSG_SIZE+50,
	     "\n   File %s contains invalid line(s)", filename);

    while(fgets(sm, BUF_SIZE, fp)) {
	sm[BUF_SIZE-1] = '\0';
	s = rmspace(sm);
	if(strlen(s) == 0 || s[0] == '#') continue;
	if(!(p = Rf_strchr(s, '='))) {
	    errs++;
	    if(strlen(msg) < MSG_SIZE) {
		strcat(msg, "\n      "); strcat(msg, s);
	    }
	    continue;
	}
	*p = '\0';
	lhs = rmspace(s);
	rhs = findterm(rmspace(p+1));
	/* set lhs = rhs */
	if(strlen(lhs) && strlen(rhs)) Putenv(lhs, rhs);
    }
    fclose(fp);
    if (errs) {
	strcat(msg, "\n   They were ignored\n");
	R_ShowMessage(msg);
    }
    return 1;
}


/* try system Renviron: R_HOME/etc/Renviron.  Unix only. */
void process_system_Renviron()
{
    char buf[PATH_MAX];

#ifdef R_ARCH
    if(strlen(R_Home) + strlen("/etc/Renviron") + strlen(R_ARCH) + 1 > PATH_MAX - 1) {
	R_ShowMessage("path to system Renviron is too long: skipping");
	return;
    }
    strcpy(buf, R_Home);
    strcat(buf, "/etc/");
    strcat(buf, R_ARCH);
    strcat(buf, "/Renviron");
#else
    if(strlen(R_Home) + strlen("/etc/Renviron") > PATH_MAX - 1) {
	R_ShowMessage("path to system Renviron is too long: skipping");
	return;
    }
    strcpy(buf, R_Home);
    strcat(buf, "/etc/Renviron");
#endif
    if(!process_Renviron(buf))
	R_ShowMessage("cannot find system Renviron");
}

#ifdef HAVE_UNISTD_H
#include <unistd.h> /* for access, R_OK */
#endif

/* try site Renviron: R_ENVIRON, then R_HOME/etc/Renviron.site. */
void process_site_Renviron ()
{
    char buf[PATH_MAX], *p = getenv("R_ENVIRON");

    if(p) {
	if(*p) process_Renviron(p);
	return;
    }
#ifdef R_ARCH
    if(strlen(R_Home) + strlen("/etc/Renviron.site") + strlen(R_ARCH) > PATH_MAX - 2) {
	R_ShowMessage("path to arch-specific Renviron.site is too long: skipping");
    } else {
	snprintf(buf, PATH_MAX, "%s/etc/%s/Renviron.site", R_Home, R_ARCH);
	if(access(buf, R_OK) == 0) {
	    process_Renviron(buf);
	    return;
	}
    }
#endif
    if(strlen(R_Home) + strlen("/etc/Renviron.site") > PATH_MAX - 1) {
	R_ShowMessage("path to Renviron.site is too long: skipping");
	return;
    }
    snprintf(buf, PATH_MAX, "%s/etc/Renviron.site", R_Home);
    process_Renviron(buf);
}

/* try user Renviron: ./.Renviron, then ~/.Renviron */
void process_user_Renviron()
{
    const char *s = getenv("R_ENVIRON_USER");

    if(s) {
	if (*s) process_Renviron(R_ExpandFileName(s));
	return;
    }

#ifdef R_ARCH
    char buff[100];
    snprintf(buff, 100, ".Renviron.%s", R_ARCH);
    if( process_Renviron(buff)) return;
#endif
    if(process_Renviron(".Renviron")) return;
#ifdef Unix
    s = R_ExpandFileName("~/.Renviron");
#endif
#ifdef Win32
    {
	char buf[1024]; /* MAX_PATH is less than this */
	/* R_USER is not necessarily set yet, so we have to work harder */
	s = getenv("R_USER");
	if(!s) s = getenv("HOME");
	if(!s) return;
	snprintf(buf, 1024, "%s/.Renviron", s);
	s = buf;
    }
#endif
#ifdef R_ARCH
    snprintf(buff, 100, "%s.%s", s, R_ARCH);
    if( process_Renviron(buff)) return;
#endif
    process_Renviron(s);
}

SEXP attribute_hidden do_readEnviron(SEXP call, SEXP op, SEXP args, SEXP env)
{

    checkArity(op, args);
    SEXP x = CAR(args);
    if (!isString(x) || LENGTH(x) != 1)
	errorcall(call, _("argument '%s' must be a character string"), "x");
    const char *fn = R_ExpandFileName(translateChar(STRING_ELT(x, 0)));
    int res = process_Renviron(fn);
    if (!res)
	warningcall(call, _("file '%s' cannot be opened for reading"), fn);
    return ScalarLogical(res != 0);
}
