/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001--2012  The R Core Team.
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

#include <R_ext/Complex.h>
#include <R_ext/RS.h>

/* use declarations in R_ext/Lapack.h (instead of having them there *and* here)
  but ``delete'' the 'extern' there : */
#define La_extern
#define BLAS_extern

#include <R_ext/Lapack.h>

#undef La_extern
#undef BLAS_extern
