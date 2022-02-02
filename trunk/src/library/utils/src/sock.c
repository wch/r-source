/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2012   The R Core Team.
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

#include <R.h>
#include <Rinternals.h>

/* from src/main/internet.c */
SEXP Rdownload(SEXP args);
SEXP Rsockconnect(SEXP sport, SEXP shost);
SEXP Rsockread(SEXP sport, SEXP smaxlen);
SEXP Rsockclose(SEXP sport);
SEXP Rsockopen(SEXP sport);
SEXP Rsocklisten(SEXP sport);
SEXP Rsockwrite(SEXP sport, SEXP sstring);

SEXP download(SEXP args)
{
    return Rdownload(CDR(args));
}

SEXP sockconnect(SEXP sport, SEXP shost)
{
    return Rsockconnect(sport, shost);
}

SEXP sockread(SEXP sport, SEXP smaxlen)
{
    return Rsockread(sport, smaxlen);
}

SEXP sockclose(SEXP sport)
{
    return Rsockclose(sport);
}

SEXP sockopen(SEXP sport)
{
    return Rsockopen(sport);
}

SEXP socklisten(SEXP sport)
{
    return Rsocklisten(sport);
}

SEXP sockwrite(SEXP sport, SEXP sstring)
{
    return Rsockwrite(sport, sstring);
}
