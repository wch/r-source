/*
 *  Copyright (C) 2008  The R Core Team
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

/* used on console.c, EncodeString and do_cat */

/* \001 is STX, \002 is ETX: they should not occur in other text,
   But we use a 3-byte escape to be even safer. */

#ifdef IN_CONSOLE
char UTF8in[4] = "\002\377\376", UTF8out[4] = "\003\377\376";
#else
extern char UTF8in[4], UTF8out[4];
#endif
