/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2004  the R Development Core Team
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <Rinternals.h>

/* defined in GNOME GUI */

int  Rgnome_ShowFiles(int nfile, char **file, char **headers, char *wtitle,
		      Rboolean del, char *pager);
int  Rgnome_ChooseFile(int new, char *buf, int len);

void Rgnome_StartConsole(Rboolean OpenConsole);
int  Rgnome_ReadConsole(char *prompt, unsigned char *buf, int len, 
			int addtohistory);
void Rgnome_WriteConsole(char *buf, int len);
void Rgnome_ResetConsole(void);
void Rgnome_FlushConsole(void);
void Rgnome_ClearerrConsole(void);
void Rgnome_loadhistory(SEXP call, SEXP op, SEXP args, SEXP env);
void Rgnome_savehistory(SEXP call, SEXP op, SEXP args, SEXP env);
