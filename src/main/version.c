/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998, The R Development Core Team
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

void PrintGreeting(void)
{
	Rprintf("\nR : Copyright %s, The R Development Core Team\n", R_YEAR);
	Rprintf("Version %s.%s %s (%s %s, %s)\n\n",
		R_MAJOR, R_MINOR, R_STATUS, R_MONTH, R_DAY, R_YEAR);

	Rprintf("R is free software and comes with ABSOLUTELY NO WARRANTY.\n");
	Rprintf("You are welcome to redistribute it under certain conditions.\n");
	Rprintf("Type\t\"?license\" or \"?licence\" for distribution details.\n\n");
	Rprintf("R is a collaborative project with many contributors.\n"
		"Type\t\"?contributors\" for a list.\n\n");

	Rprintf("Type\t\"demo()\" for some demos,"
		" \"help()\" for on-line help, or\n\t\"help.start()\""
		" for a HTML browser interface to help.\n\n");
}

SEXP do_version(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP a, ans;
	char buf[128];
	checkArity(op, args);

	PROTECT(a = ans = allocList(12));
	TAG(a) = install("platform");
	CAR(a) = mkString(R_PLATFORM);
	a= CDR(a); TAG(a) = install("arch");	CAR(a) = mkString(R_CPU);
	a= CDR(a); TAG(a) = install("os");	CAR(a) = mkString(R_OS);
	a= CDR(a); sprintf(buf,"%s, %s", R_CPU, R_OS);
	TAG(a) = install("system");		CAR(a) = mkString(buf);
	a= CDR(a); TAG(a) = install("status");	CAR(a) = mkString(R_STATUS);
	a= CDR(a); TAG(a)=install("status.rev");CAR(a) = mkString(R_STATUS_REV);
	a= CDR(a); TAG(a) = install("major");	CAR(a) = mkString(R_MAJOR);
	a= CDR(a); TAG(a) = install("minor");	CAR(a) = mkString(R_MINOR);
	a= CDR(a); TAG(a) = install("year");	CAR(a) = mkString(R_YEAR);
	a= CDR(a); TAG(a) = install("month");	CAR(a) = mkString(R_MONTH);
	a= CDR(a); TAG(a) = install("day");	CAR(a) = mkString(R_DAY);
	a= CDR(a); TAG(a) = install("language");CAR(a) = mkString("R");
	UNPROTECT(1);
	return ans;
}
