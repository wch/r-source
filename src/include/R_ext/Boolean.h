/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000, 2025 The R Core Team.
 *
 *  This header file is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This file is part of R. R is distributed under the terms of the
 *  GNU General Public License, either Version 2, June 1991 or Version 3,
 *  June 2007. See doc/COPYRIGHTS for details of the copyright status of R.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/* Included by R.h: Part of the API. */

#ifndef R_EXT_BOOLEAN_H_
#define R_EXT_BOOLEAN_H_

#undef FALSE
#undef TRUE



#ifdef  __cplusplus

extern "C" {
    /* once cpp11 is sorted
typedef bool Rboolean;
#define FALSE false
#define TRUE true
    */
typedef enum { FALSE = 0, TRUE } Rboolean;
}

# else

// Also include standard C boolean type
#if defined __STDC_VERSION__ && __STDC_VERSION__ > 201710L
// C23 so these are keywords
#else
// stdbool.h is C99, so available everywhere
//# include <stdbool.h>
#endif

/* 
   __bool_true_false_are_defined is defined in stdbool.h, and C23, but
   it and stdbool.h are declared obsolescent in C23.
*/
#ifdef __bool_true_false_are_defined
typedef bool Rboolean;
# define FALSE false
# define TRUE true
# define _R_RBOOLEAN_IS_BOOL_ 1
#else
typedef enum { FALSE = 0, TRUE } Rboolean;
#endif //__bool_true_false_are_defined

# endif // __cplusplus


#endif /* R_EXT_BOOLEAN_H_ */
