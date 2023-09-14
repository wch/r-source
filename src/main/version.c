/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2023  The R Core Team
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>
#include <Rversion.h>

attribute_hidden void PrintGreeting(void)
{
    char buf[384];

    Rprintf("\n");
    PrintVersion_part_1(buf, 384);
    Rprintf("%s\n", buf);

    Rprintf(_("R is free software and comes with ABSOLUTELY NO WARRANTY.\n\
You are welcome to redistribute it under certain conditions.\n\
Type 'license()' or 'licence()' for distribution details.\n\n"));
    Rprintf(_("R is a collaborative project with many contributors.\n\
Type 'contributors()' for more information and\n\
'citation()' on how to cite R or R packages in publications.\n\n"));
    Rprintf(_("Type 'demo()' for some demos, 'help()' for on-line help, or\n\
'help.start()' for an HTML browser interface to help.\n\
Type 'q()' to quit R.\n\n"));
}

attribute_hidden SEXP do_version(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP value, names;
    char buf[128];
    int i = 0;

    checkArity(op, args);
#ifndef Win32
    PROTECT(value = allocVector(VECSXP,14));
    PROTECT(names = allocVector(STRSXP,14));
#else
    PROTECT(value = allocVector(VECSXP,15));
    PROTECT(names = allocVector(STRSXP,15));
#endif

    SET_STRING_ELT(names, i, mkChar("platform"));
    SET_VECTOR_ELT(value, i++, mkString(R_PLATFORM));
    SET_STRING_ELT(names, i, mkChar("arch"));
    SET_VECTOR_ELT(value, i++, mkString(R_CPU));
    SET_STRING_ELT(names, i, mkChar("os"));
    SET_VECTOR_ELT(value, i++, mkString(R_OS));
#ifdef Win32
    SET_STRING_ELT(names, i, mkChar("crt"));
    SET_VECTOR_ELT(value, i++, mkString(R_CRT));
#endif

    snprintf(buf, 128, "%s, %s", R_CPU, R_OS);
    SET_STRING_ELT(names, i, mkChar("system"));
    SET_VECTOR_ELT(value, i++, mkString(buf));

    SET_STRING_ELT(names, i, mkChar("status"));
    SET_VECTOR_ELT(value, i++, mkString(R_STATUS));
    SET_STRING_ELT(names, i, mkChar("major"));
    SET_VECTOR_ELT(value, i++, mkString(R_MAJOR));
    SET_STRING_ELT(names, i, mkChar("minor"));
    SET_VECTOR_ELT(value, i++, mkString(R_MINOR));
    SET_STRING_ELT(names, i, mkChar("year"));
    SET_VECTOR_ELT(value, i++, mkString(R_YEAR));
    SET_STRING_ELT(names, i, mkChar("month"));
    SET_VECTOR_ELT(value, i++, mkString(R_MONTH));
    SET_STRING_ELT(names, i, mkChar("day"));
    SET_VECTOR_ELT(value, i++, mkString(R_DAY));

    SET_STRING_ELT(names, i, mkChar("svn rev"));
    snprintf(buf, 128, "%d", R_SVN_REVISION);
    SET_VECTOR_ELT(value, i++, mkString(buf));
    SET_STRING_ELT(names, i, mkChar("language"));
    SET_VECTOR_ELT(value, i++, mkString("R"));

    PrintVersionString(buf, 128);
    SET_STRING_ELT(names, i, mkChar("version.string"));
    SET_VECTOR_ELT(value, i++, mkString(buf));
    SET_STRING_ELT(names, i, mkChar("nickname"));
    SET_VECTOR_ELT(value, i++, mkString(R_NICK));

    setAttrib(value, R_NamesSymbol, names);
    UNPROTECT(2);
    return value;
}

attribute_hidden void PrintVersion(char *s, size_t len)
{
    PrintVersion_part_1(s, len);

    strcat(s, "\n"
	   "R is free software and comes with ABSOLUTELY NO WARRANTY.\n"
	   "You are welcome to redistribute it under the terms of the\n"
	   "GNU General Public License versions 2 or 3.\n"
	   "For more information about these matters see\n"
	   "https://www.gnu.org/licenses/.\n");
}

attribute_hidden void PrintVersionString(char *s, size_t len)
{
    
#ifndef Win32
# define _R_PV_EXTRA_ ""
#else
# define _R_PV_EXTRA_ " " R_CRT
#endif
    if(R_SVN_REVISION <= 0) {// 'svn info' failed in ../../Makefile.in
	snprintf(s, len, "R version %s.%s %s (%s-%s-%s%s)",
		R_MAJOR, R_MINOR, R_STATUS, R_YEAR, R_MONTH, R_DAY,
	        _R_PV_EXTRA_);
    } else if(strlen(R_STATUS) == 0) {
	snprintf(s, len, "R version %s.%s (%s-%s-%s%s)",
		R_MAJOR, R_MINOR, R_YEAR, R_MONTH, R_DAY, _R_PV_EXTRA_);
    } else if(strcmp(R_STATUS, "Under development (unstable)") == 0) {
	snprintf(s, len, "R %s (%s-%s-%s r%d%s)",
		R_STATUS, R_YEAR, R_MONTH, R_DAY, R_SVN_REVISION,
	        _R_PV_EXTRA_);
    } else {
	snprintf(s, len, "R version %s.%s %s (%s-%s-%s r%d%s)",
		R_MAJOR, R_MINOR, R_STATUS, R_YEAR, R_MONTH, R_DAY,
		R_SVN_REVISION, _R_PV_EXTRA_);
    }
#undef _R_PV_EXTRA_
}

attribute_hidden void PrintVersion_part_1(char *s, size_t len)
{
#define SPRINTF_2(_FMT, _OBJ) snprintf(tmp, 128, _FMT, _OBJ); strcat(s, tmp)
    char tmp[128];

    PrintVersionString(s, len);
    if(strlen(R_NICK) != 0) {
	char nick[128];
	snprintf(nick, 128, " -- \"%s\"", R_NICK);
	strcat(s, nick);
    }
    SPRINTF_2("\nCopyright (C) %s The R Foundation for Statistical Computing\n",
	      R_YEAR);
    SPRINTF_2("Platform: %s", R_PLATFORM);
#ifdef R_ARCH
    if(strlen(R_ARCH)) { SPRINTF_2("/%s", R_ARCH); }
#endif
    if(sizeof(void *) != 8) {
	SPRINTF_2(" (%d-bit)", 8*(int)sizeof(void *));
    }
    strcat(s, "\n");
}

attribute_hidden SEXP do_internalsID(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return mkString(R_INTERNALS_UUID);
}
