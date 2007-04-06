/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file rt_complete.c
 *  Copyright (C) 2007 The R Core Developmen Team.
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


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <getline/getline.h>
#include <strings.h>
extern char *alloca(size_t);
#define min(a, b) (a < b ? a : b)

#include <Rinternals.h>
#include <R_ext/Parse.h>

static int rcompgen_available = -1;

static int rt_completion(char *buf, int offset, int *loc)
{
    int i, alen, cursor_position = *loc;
    char *partial_line = buf;
    char *additional_text;
    char *pline, *cmd;
    SEXP cmdSexp, cmdexpr, ans = R_NilValue;
    ParseStatus status;

    if(!rcompgen_available) return *loc;
    
    if(rcompgen_available < 0) {
	char *p = getenv("R_COMPLETION");
	if(p && strcmp(p, "FALSE") == 0) {
	    rcompgen_available = 0;
	    return -1; /* no change */	    
	}
	/* First check if namespace is loaded */
	if(findVarInFrame(R_NamespaceRegistry, install("rcompgen"))
	   != R_UnboundValue) rcompgen_available = 1;
	else { /* Then try to load it */
	    char *p = "try(loadNamespace('rcompgen'), silent=TRUE)";
	    PROTECT(cmdSexp = mkString(p));
	    cmdexpr = PROTECT(R_ParseVector(cmdSexp, -1, &status, R_NilValue));
	    if(status == PARSE_OK) {
		for(i = 0; i < length(cmdexpr); i++)
		    eval(VECTOR_ELT(cmdexpr, i), R_GlobalEnv);
	    }
	    UNPROTECT(2);
	    if(findVarInFrame(R_NamespaceRegistry, install("rcompgen"))
	       != R_UnboundValue) rcompgen_available = 1;
	    else {
		rcompgen_available = 0;
		return -1; /* no change */
	    }
	}
    }

    /* FIXME: need to escape quotes properly */
    pline = alloca(strlen(partial_line) + 1);
    strcpy(pline, partial_line);
    /* poor attempt at escaping quotes that sort of works */
    alen = strlen(pline);
    for (i = 0; i < alen; i++)
        if (pline[i] == '"') pline[i] = '\'';

    cmd = alloca(strlen(pline) + 100);
    sprintf(cmd, "rcompgen:::.win32consoleCompletion(\"%s\", %d)",
	    pline, cursor_position);
    PROTECT(cmdSexp = mkString(cmd));
    cmdexpr = PROTECT(R_ParseVector(cmdSexp, -1, &status, R_NilValue));
    if (status != PARSE_OK) {
	UNPROTECT(2);
	/* Uncomment next line to debug */
	/* Rprintf("failed: %s \n", cmd); */
	/* otherwise pretend that nothing happened and return */
	return -1; /* no change */
    }
    /* Loop is needed here as EXPSEXP will be of length > 1 */
    for(i = 0; i < length(cmdexpr); i++)
	ans = eval(VECTOR_ELT(cmdexpr, i), R_GlobalEnv);
    UNPROTECT(2);
    
    /* ans has the form list(addition, possible), where 'addition' is
       unique additional text if any, and 'possible' is a character
       vector holding possible completions if any (already formatted
       for linewise printing in the current implementation).  If
       'possible' has any content, we want to print those (or show in
       status bar or whatever).  Otherwise add the 'additional' text
       at the cursor */

#define ADDITION 0
#define POSSIBLE 1

    alen = length(VECTOR_ELT(ans, POSSIBLE));
    if (alen) {
	int max_show = 10;
	printf("\n"); /* finish current line */
	for (i = 0; i < min(alen, max_show); i++) {
	    printf("%s\n", CHAR(STRING_ELT(VECTOR_ELT(ans, POSSIBLE), i)));
	}
	if (alen > max_show)
	    printf("\n[...truncated]\n");
	cursor_position = -2; /* Need to redisplay whole line */
    }
    additional_text = CHAR(STRING_ELT( VECTOR_ELT(ans, ADDITION), 0 ));
    alen = strlen(additional_text);
    if (alen) {
	int cp = *loc;
	memcpy(buf+cp, additional_text, alen+1);
	*loc = cp + alen;
    }
    return cursor_position;
}


void R_gl_tab_set(void)
{
    gl_tab_hook = rt_completion;
}
