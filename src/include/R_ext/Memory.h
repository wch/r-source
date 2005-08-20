/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2005    Robert Gentleman, Ross Ihaka 
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 * Memory Allocation (garbage collected) --- INCLUDING S compatibility ---
 */

#ifndef R_EXT_MEMORY_H_
#define R_EXT_MEMORY_H_

#ifdef  __cplusplus
extern "C" {
#endif

char*	vmaxget(void);
void	vmaxset(char*);

void	R_gc(void);

/* <FIXME> this really needs to be R_size_t, since Win64 has a 32-bit
   long.  See comments in memory.c
*/
char*	R_alloc(long, int);
char*	S_alloc(long, int);
char*	S_realloc(char*, long, long, int);

#ifdef  __cplusplus
}
#endif

#endif /* R_EXT_MEMORY_H_ */
