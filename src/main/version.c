/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998, 2000  The R Development Core Team
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include "Rversion.h"

void PrintGreeting(void)
{
    Rprintf("\nR : Copyright %s, The R Development Core Team\n", R_YEAR);
    Rprintf("Version %s.%s %s (%s %s, %s)\n\n",
	    R_MAJOR, R_MINOR, R_STATUS, R_MONTH, R_DAY, R_YEAR);

    Rprintf("R is free software and comes with ABSOLUTELY NO WARRANTY.\n"
	    "You are welcome to redistribute it under certain conditions.\n"
	    "Type\t\"?license\" or \"?licence\" for distribution details.\n\n");
    Rprintf("R is a collaborative project with many contributors.\n"
	    "Type\t\"?contributors\" for a list.\n\n");

    Rprintf("Type\t\"demo()\" for some demos, \"help()\" for on-line help, or\n"
            "    \t\"help.start()\" for a HTML browser interface to help.\n"
	    "Type\t\"q()\" to quit R.\n\n");
}

SEXP do_version(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP value, names;
    char buf[128];
    checkArity(op, args);
    sprintf(buf,"%s, %s", R_CPU, R_OS);
    PROTECT(value = allocVector(VECSXP,11));
    PROTECT(names = allocVector(STRSXP,11));
    STRING(names)[0]  = mkChar("platform");
    VECTOR(value)[0]  = mkString(R_PLATFORM);
    STRING(names)[1]  = mkChar("arch");
    VECTOR(value)[1]  = mkString(R_CPU);
    STRING(names)[2]  = mkChar("os");
    VECTOR(value)[2]  = mkString(R_OS);
    STRING(names)[3]  = mkChar("system");
    VECTOR(value)[3]  = mkString(buf);
    STRING(names)[4]  = mkChar("status");
    VECTOR(value)[4]  = mkString(R_STATUS);
    STRING(names)[5]  = mkChar("major");
    VECTOR(value)[5]  = mkString(R_MAJOR);
    STRING(names)[6]  = mkChar("minor");
    VECTOR(value)[6]  = mkString(R_MINOR);
    STRING(names)[7]  = mkChar("year");
    VECTOR(value)[7]  = mkString(R_YEAR);
    STRING(names)[8]  = mkChar("month");
    VECTOR(value)[8]  = mkString(R_MONTH);
    STRING(names)[9] = mkChar("day");
    VECTOR(value)[9] = mkString(R_DAY);
    STRING(names)[10] = mkChar("language");
    VECTOR(value)[10] = mkString("R");
    setAttrib(value, R_NamesSymbol, names);
    UNPROTECT(2);
    return value;
}

void PrintVersion(char *s)
{
    char tmp[50];
    sprintf(s, "R %s.%s %s (%s %s, %s)\n",
	    R_MAJOR, R_MINOR, R_STATUS, R_MONTH, R_DAY, R_YEAR);
    sprintf(tmp, "Copyright (C) %s R Development Core Team\n\n", R_YEAR);
    strcat(s, tmp);
    strcat(s, "R is free software and comes with ABSOLUTELY NO WARRANTY.\n");
    strcat(s, "You are welcome to redistribute it under the terms of the\n");
    strcat(s, "GNU General Public License.  For more information about\n");
    strcat(s, "these matters, see http://www.gnu.org/copyleft/gpl.html.\n");
}
