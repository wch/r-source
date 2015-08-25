/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2015 R Core Team
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
 
/* Recent MinGW-w64 releases define snprintf and vsnprintf as macros
   that can't be redefined; these are necessary to make our substitutions
   of the trio replacements.  We need the replacements because msvcrt.dll
   doesn't support hex formatting of floating point numbers, and all (?)
   versions of the MS run-time use three digits in floating point
   E formats, e.g. 1.E000. */

#ifndef R_TRIOREMAP_H
#define R_TRIOREMAP_H

int trio_snprintf(char *buffer, size_t max, const char *format, ...);
int trio_vsnprintf(char *buffer, size_t bufferSize, const char *format,
		   va_list args);
#define snprintf trio_snprintf
#define vsnprintf trio_vsnprintf

#endif  /* not R_TRIOREMAP_H */
