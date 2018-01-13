/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2018 The R Core Team
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

/* This appears not to currently be needed: in 2018-01 no Fortran I/O
 * is done.  But left as future-proofing and as an example (mentioned
 * in 'Writing R Extensions').
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef _WIN32
# include <fcntl.h>

static void __attribute__((constructor))
init(void)
{
    /* gfortran initialization sets these to _O_BINARY */
    setmode(1,_O_TEXT); /* stdout */
    setmode(2,_O_TEXT); /* stderr */
    return;
}
#endif
