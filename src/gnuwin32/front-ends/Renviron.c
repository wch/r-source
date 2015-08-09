/*
 *   R : A Computer Language for Statistical Data Analysis
 *   Copyright (C) 1997-2008   The R Core Team
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

#include <stdlib.h> /* for setenv or putenv */
#define alloca(x) __builtin_alloca((x))
#include <stdio.h>
#include <string.h>
#include <ctype.h>

/* remove leading and trailing space */
static char *rmspace(char *s)
{
    int   i;

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
    p = strchr(s, '-');
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
	pl = strchr(p, '{');
	pr = strchr(p, '}');
	if(!pr) return NULL;
	if(!pl || pr < pl) {
	    p = pr+1; nr++;
	} else {
	    p = pl+1; nl++;
	}
    }
    return pr;
}


static char *findterm(char *s)
{
    char *p, *q, *r, *r2, *ss=s;
    static char ans[1000];
    int nans;

    if(!strlen(s)) return "";
    ans[0] = '\0';
    while(1) {
	/* Look for ${...}, taking care to look for inner matches */
	p = strchr(s, '$');
	if(!p || p[1] != '{') break;
	q = findRbrace(p+2);
	if(!q) break;
	/* copy over leading part */
	nans = strlen(ans);
	strncat(ans, s, p-s); ans[nans + p - s] = '\0';
	r = (char *) alloca(q - p + 2);
	strncpy(r, p, q - p + 1);
	r[q - p + 1] = '\0';
	r2 = subterm(r);
	if(strlen(ans) + strlen(r2) < 1000) strcat(ans, r2); else return ss;
	/* now repeat on the tail */
	s = q+1;
    }
    if(strlen(ans) + strlen(s) < 1000) strcat(ans, s); else return ss;
    return ans;
}

static void Putenv(char *a, char *b)
{
    char *buf, *value, *p, *q, quote='\0';
    int inquote = 0;

    buf = (char *) malloc((strlen(a) + strlen(b) + 2) * sizeof(char));
    if(!buf) {
	fprintf(stderr, "allocation failure in reading Renviron");
	return;
    }
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
    if(putenv(buf))
	fprintf(stderr, "Problem in setting variable '%s' in Renviron", a);
    /* no free here: storage remains in use */
}


#define BUF_SIZE 255
#define MSG_SIZE 2000
int process_Renviron(const char *filename)
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
	if(!(p = strchr(s, '='))) {
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
	fprintf(stderr, "%s", msg);
    }
    return 1;
}
