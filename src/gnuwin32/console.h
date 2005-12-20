/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file console.h
 *  Copyright (C) 1998--2003  Guido Masarotto and Brian Ripley
 *  Copyright (C) 2004	      The R Foundation
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

typedef window console;
typedef window pager;
typedef window dataeditor;
typedef window editor;

void
setconsoleoptions(char *fnname,int fnsty, int fnpoints,
		  int rows, int cols, int consx, int consy,
		  rgb nfg, rgb nufg, rgb nbg, rgb high,
		  int pgr, int pgc, int multiplewindows, int widthonresize,
		  int bufbytes, int buflines);
pager newpager(char *title, char *filename, char *header, int unlinkonexit);
console newconsole(char *name, int flags);
int  consolereads(console c, char *prompt, char *buf, int len,
		  int addtohistory);
int  consolewrites(console c, char *s);
int  consolecancopy(console c);
int  consolecanpaste(console c);
void consolecopy(console c);
void consolepaste(console c);
void consolepastecmds(console c);
void consoleselectall(console c);
void consolecmd(console c, char *cmd);
void consolenewline(console c);
void consolehelp();
void consolesetbrk(console c, actionfn, char ch, char mod);
void consoletogglelazy(console c);
int  consolegetlazy(console c);
void consoleflush(console c);
void consoleprint(console c);
void consolesavefile(console c, int pager);
void drawconsole(control c, rect r);
void consoleclear(control c);

extern int setWidthOnResize;
int consolecols(console c);
void pagerbclose(control m);
