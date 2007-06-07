/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2001    Robert Gentleman, Ross Ihaka
 *                             and the R Development Core Team
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */
#ifndef R_EXT_PRINT_H_
#define R_EXT_PRINT_H_

#include <stdarg.h>

#ifdef  __cplusplus
extern "C" {
#endif

void Rprintf(char *, ...);
void REprintf(char *, ...);
void Rvprintf(const char *, va_list);
void REvprintf(const char *, va_list);

#ifdef  __cplusplus
}
#endif

#endif /* R_EXT_PRINT_H_ */
