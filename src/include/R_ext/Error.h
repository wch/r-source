/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2005   The R Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/* Included by R.h: API */

#ifndef R_ERROR_H_
#define R_ERROR_H_

#ifdef  __cplusplus
extern "C" {
#endif

/* valid from gcc 2.95.3, at least.
   Suggested by Anton Korobeynikov.
 */
#if defined(__GNUC__) && __GNUC__ >= 3
void Rf_error(const char *, ...) __attribute__((noreturn));
void UNIMPLEMENTED(const char *) __attribute__((noreturn));
void WrongArgCount(const char *) __attribute__((noreturn));
#else
void Rf_error(const char *, ...);
void UNIMPLEMENTED(const char *);
void WrongArgCount(const char *);
#endif

void	Rf_warning(const char *, ...);
void 	R_ShowMessage(const char *s);
    

#ifdef  __cplusplus
}
#endif

#ifndef R_NO_REMAP
#define error Rf_error
#define warning Rf_warning
#endif


#endif /* R_ERROR_H_ */
