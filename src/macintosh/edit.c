/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

#include "Defn.h"
#include "Print.h"

/*
 * ed, vi etc have 3 parameters. the data, a file and an editor
 * 
 * if file is specified then the given file is used (and not removed on exit) if
 * file is not specified then a temporary file is used; since only one
 * temporary file is used for an entire session previous editing is lost
 * 
 * if data is specified then it is passed out to be edited; if data is not
 * specified then either file (if specified) or the temporary file is used
 * (thus errors can be re-editied by calling edit a second time with no
 * arguments).
 * 
 * if the editor is specified then the specified editor is invoked if possible
 * and an error message reported otherwise
 */

void InitEd()
{
}

void MacTextEdit(char *inbuf, int insize, char **outbuf, int *outsize, int *status)
{
	int i;
	char *p;

	p = *outbuf = malloc(insize);
	for(i=0 ; i<insize ; i++) {
		p[i] = inbuf[i];
	}
	*outsize = insize;
}

SEXP do_edit(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	int i, j, l, protected, insize, outsize, status;
	SEXP x, s, envir;
	char *inbuf, *outbuf, *p, *q;
	char *vmaxsave;

	checkArity(op, args);
	vmaxsave = vmaxget();

	x = CAR(args);
	if (TYPEOF(x) == CLOSXP) {
		PROTECT(envir = CLOENV(x));
		protected = 1;
	}
	else protected = 0;

	if (x != R_NilValue) {
		x = deparse1(x, 0);
		insize = 0;
		for (i=0; i<LENGTH(x); i++)
			insize += strlen(CHAR(STRING(x)[i]))+1;
		p = inbuf = malloc(insize);
		for (i=0; i<LENGTH(x); i++) {
			q = CHAR(STRING(x)[i]);
			while(*q) *p++ = *q++;
			*p++ = '\r';
		}

		MacTextEdit(inbuf, insize, &outbuf, &outsize, &status);

			/* Count the number of strings */
			/* Note that we take \r as a SEPARATOR */

		p = q = outbuf;
		insize = 0;
		for(i=0 ; i<outsize ; i++)
			if(p[i] == '\r') insize++;
		PROTECT(x = allocVector(STRSXP, insize));
		protected++;

			/* put the strings in place */

		insize = 0;
		for(i=0 ; i<outsize ; i++) {
			if(*p == '\r') {
				l = p - q;
				s = allocString(l);
				for(j=0 ; j<l ; j++)
					CHAR(s)[j] = q[j];
				CHAR(s)[l] = '\0';
				STRING(x)[insize++] = s;
				q = p+1;
			}
			*p++;
		}

			/* cleanup malloc-ed space */

		free(inbuf);
		free(outbuf);

			/* parse the edited text */
	}
	UNPROTECT(protected);
	return x;
}
