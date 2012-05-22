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
 *  http://www.r-project.org/Licenses/
 */

/* Declarations for .Call entry points */

SEXP Cdqrls(SEXP x, SEXP y, SEXP tol);
SEXP Cdist(SEXP x, SEXP method, SEXP attrs, SEXP p);
SEXP cor(SEXP x, SEXP y, SEXP na_method, SEXP method);
SEXP cov(SEXP x, SEXP y, SEXP na_method, SEXP method);
SEXP updateform(SEXP old, SEXP new);
SEXP fft(SEXP z, SEXP inverse);
SEXP mvfft(SEXP z, SEXP inverse);
SEXP nextn(SEXP n, SEXP factors);

/* Declarations for .External[2] entry points */

SEXP compcases(SEXP args);
SEXP doD(SEXP args);
SEXP deriv(SEXP args);
SEXP modelframe(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP modelmatrix(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP termsform(SEXP args);
SEXP do_fmin(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP nlm(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP zeroin2(SEXP call, SEXP op, SEXP args, SEXP rho);
