/*
  R : A Computer Language for Statistical Data Analysis
  Copyright (C) 1997-2004   Robert Gentleman, Ross Ihaka
                            and the R Development Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at
  your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307,
  U.S.A.
 */

/* <UTF8> This does byte-level access, e.g. isspace, but is OK. */

/* ------------------- process .Renviron files in C -----------------
 *  Formerly part of ../unix/sys-common.c. 
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <Defn.h> /* for PATH_MAX */


/* remove leading and trailing space */
static char *rmspace(char *s)
{
    int   i;

    for (i = strlen(s) - 1; i >= 0 && isspace((int)s[i]); i--) s[i] = '\0';
    for (i = 0; isspace((int)s[i]); i++);
    return s + i;
}

/* look for ${FOO:-bar} constructs, recursively */
static char *findterm(char *s)
{
    char *p, *q;

    if(!strlen(s)) return "";
    if(strncmp(s, "${", 2)) return s;
    /* found one, so remove leading ${ and final } */
    if(s[strlen(s) - 1] != '}') return "";
    s[strlen(s) - 1] = '\0';
    s += 2;
    p = Rf_strchr(s, '-');
    if(!p) return "";
    q = p + 1; /* start of value */
    if(p - s > 1 && *(p-1) == ':') *(p-1) = '\0'; else *p = '\0';
    s = rmspace(s);
    if(!strlen(s)) return "";
    p = getenv(s);
    if(p && strlen(p)) return p; /* variable was set and non-empty */
    return findterm(q);
}

static void Putenv(char *a, char *b)
{
    char *buf, *value, *p, *q, quote='\0';
    int inquote = 0;

    buf = (char *) malloc((strlen(a) + strlen(b) + 2) * sizeof(char));
    if(!buf) R_Suicide("allocation failure in reading Renviron");
    strcpy(buf, a); strcat(buf, "=");
    value = buf+strlen(buf);

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
    putenv(buf);
    /* no free here: storage remains in use */
}


#define BUF_SIZE 255
#define MSG_SIZE 2000
static int process_Renviron(char *filename)
{
    FILE *fp;
    char *s, *p, sm[BUF_SIZE], *lhs, *rhs, msg[MSG_SIZE+50];
    int errs = 0;

    if (!filename || !(fp = fopen(filename, "r"))) return 0;
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

    if(strlen(R_Home) + strlen("/etc/Renviron") > PATH_MAX - 1) {
	R_ShowMessage("path to system Renviron is too long: skipping");
	return;
    }
    strcpy(buf, R_Home);
    strcat(buf, "/etc/Renviron");
    if(!process_Renviron(buf))
	R_ShowMessage("cannot find system Renviron");
}

/* try site Renviron: R_ENVIRON, then R_HOME/etc/Renviron.site. */
void process_site_Renviron ()
{
    char buf[PATH_MAX], *p = getenv("R_ENVIRON");

    if(p && strlen(p)) {
	process_Renviron(p);
	return;
    }
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
    char *s;

    if(process_Renviron(".Renviron")) return;
#ifdef Unix
    s = R_ExpandFileName("~/.Renviron");
#endif
#ifdef Win32
    {
	char buf[1024];
	/* R_USER is not necessarily set yet, so we have to work harder */
	s = getenv("R_USER");
	if(!s) s = getenv("HOME");
	if(!s) return;
	snprintf(buf, 1024, "%s/.Renviron", s);
	s = buf;
    }
#endif
    process_Renviron(s);
}
