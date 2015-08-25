/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2011  The R Core Team
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

extern int R_SaveAsPng(void  *d, int width, int height,
		       unsigned int (*gp)(void *, int, int),
		       int bgr, FILE *fp, unsigned int transparent, int res);

extern int R_SaveAsJpeg(void  *d, int width, int height,
			unsigned int (*gp)(void *, int, int),
			int bgr, int quality, FILE *outfile, int res);

extern int R_SaveAsTIFF(void  *d, int width, int height,
			unsigned int (*gp)(void *, int, int),
			int bgr, const char *outfile, int res,
			int compression);

extern int R_SaveAsBmp(void  *d, int width, int height,
		       unsigned int (*gp)(void *, int, int),
		       int bgr, FILE *fp, int res);
