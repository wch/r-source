/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1998, 1999  Guido Masarotto and Brian Ripley
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

#define RW_MDI         0x0001
#define RW_TOOLBAR     0x0010
#define RW_STATUSBAR   0x0100
#define RW_LARGEICONS   0x1000

extern int RguiMDI;
extern int MDIset;
extern window RConsole;
void Rconsolecmd(char *);

void R_ShowMessage(char *s);
/*void (*R_ShowMessage)(char *s);*/
int (*R_yesnocancel)(char *s);



