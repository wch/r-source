/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2002  The R Development Core Team.
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
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdlib.h>
#include <stdarg.h>

int vsnprintf(char *s, size_t n, const char *format, va_list ap);

int snprintf (char *s, size_t n, const char *format, ...)
{
    int total;
    va_list ap;

    va_start(ap, format);
    total = vsnprintf(s, n, format, ap);
    va_end(ap);
    return total;
}
