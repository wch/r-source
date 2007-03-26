/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-2000  Robert Gentleman, Ross Ihaka and the
 *                           R Development Core Team
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
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
 */

/*--------------------------------------------------------------------------*/
/* Formerly in gram.y */

/* Basic File IO : This code is here because at this particular instant */
/* it seems closely related to cget(), which appears below.  But now it */
/* doesn't. */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"

int R_fgetc(FILE *fp)
{
#ifdef Win32
    int c;
    static int nexteof=0;
    if (nexteof) {
       nexteof = 0;
       return R_EOF;
    }
    c = fgetc(fp);
    if (c==EOF) {
       nexteof = 1;
       return '\n';
    }
#else
    int c = fgetc(fp);
#endif
    /* get rid of  CR in CRLF line termination */
    if (c == '\r') {
	c = fgetc(fp);
	/* retain CR's with no following linefeed */
	if (c != '\n') {
	    ungetc(c,fp);
	    return('\r');
	}
    }
#ifdef Win32
    return c;
#else
    return feof(fp) ? R_EOF : c;
#endif
}


