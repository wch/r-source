/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2001   Robert Gentleman, Ross Ihaka 
 *                            and the R Development Core Team
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
 */

#ifndef _R_ERROR_H_
#define _R_ERROR_H_

#ifndef R_NO_REMAP
#define error Rf_error
#define warning Rf_warning
#endif

#ifdef  __cplusplus
extern "C" {
#endif

void	error(const char *, ...);
void	warning(const char *, ...);
void	WrongArgCount(char *);
void	UNIMPLEMENTED(char *);

#ifdef  __cplusplus
}
#endif

#endif /* _R_ERROR_H_ */
