/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1998--1999  Guido Masarotto and Brian Ripley
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

#ifdef HAVE_CONFIG_H
#include <Rconfig.h>
#endif

/* R user interface based on GraphApp */
#include "Defn.h"
#include <stdio.h>
#include "graphapp/ga.h"
#include "graphapp/stdimg.h"
#include "console.h"
#include "rui.h"
#include "opt.h"
#include "Rversion.h"

#define TRACERUI(a)

extern int UserBreak;

console RConsole = NULL;
int   RguiMDI = RW_MDI | RW_TOOLBAR | RW_STATUSBAR;
int   MDIset = 0;
static window RFrame;
extern int ConsoleAcceptCmd;
static menubar RMenuBar;
static menuitem msource, mdisplay, mload, msave, mpaste, mcopy, 
    mcopypaste, mlazy;
static menuitem mls, mrm, msearch, mhelp, /*mmanmain,*/ mmanref, 
    mmanext, mapropos, mhelpstart;
static menu m;
static char cmd[1024];

/* menu callbacks */

void fixslash(char *s)
{
    char *p;

    for (p = s; *p; p++)
	if (*p == '\\') *p = '/';
/* I don't know why we need this!!!! */
    if (!strcmp(&s[strlen(s) - 2], ".*"))
	s[strlen(s) - 2] = '\0';
}

void Rconsolecmd(char *cmd)
{
    consolecmd(RConsole, cmd);
}

static void menusource(control m)
{
    char *fn;

    if (!ConsoleAcceptCmd) return;
    setuserfilter("R files (*.R)\0*.R\0S files (*.q)\0*.q\0All files (*.*)\0*.*\0\0");
    fn = askfilename("Select file to source", "");
    show(RConsole);
    if (fn) {
	fixslash(fn);
	sprintf(cmd, "source(\"%s\")", fn);
	consolecmd(RConsole, cmd);
    }
}

static void menudisplay(control m)
{
    char *fn;

    if (!ConsoleAcceptCmd) return;
    setuserfilter("All files (*.*)\0*.*\0\0");
    fn = askfilename("Select file to show", "");
    show(RConsole);
    if (fn) {
	fixslash(fn);
	sprintf(cmd, 
		"file.show(\"%s\", title=\"File\", header=\"%s\")",
		fn, fn);
	consolecmd(RConsole, cmd);
    }
}

static void menuloadimage(control m)
{
    char *fn;

    if (!ConsoleAcceptCmd) return;
    setuserfilter("R images (*.RData)\0*.RData\0R images - old extension (*.rda)\0*.rda\0All files (*.*)\0*.*\0\0");
    fn = askfilename("Select image to load", "");
    show(RConsole);
    if (fn) {
	fixslash(fn);
	sprintf(cmd, "load(\"%s\")", fn);
	consolecmd(RConsole, cmd);
    }
}

static void menusaveimage(control m)
{
    char *fn;

    if (!ConsoleAcceptCmd) return;
    setuserfilter("R images (*.RData)\0*.RData\0All files (*.*)\0*.*\0\0");
    fn = askfilesave("Save image in", "");
    show(RConsole);
    if (fn) {
	fixslash(fn);
	sprintf(cmd, "save.image(\"%s\")", fn);
	consolecmd(RConsole, cmd);
    }
}

static void menuchangedir(control m)
{
    askchangedir();
    show(RConsole);
}

static void menuprint(control m)
{
    consoleprint(RConsole);
    show(RConsole);
}

static void menuexit(control m)
{
    R_CleanUp(SA_DEFAULT, 0, 1);
}

static void menuselectall(control m)
{
    consoleselectall(RConsole);
    show(RConsole);
}

static void menucopy(control m)
{
    if (consolecancopy(RConsole))
	consolecopy(RConsole);
    else
	askok("No selection");
    show(RConsole);
}

static void menupaste(control m)
{
    if (consolecanpaste(RConsole))
	consolepaste(RConsole);
    else
	askok("No text available");
    show(RConsole);
}

static void menucopypaste(control m)
{
    if (consolecancopy(RConsole)) {
	consolecopy(RConsole);
	consolepaste(RConsole);
    } else
	askok("No selection");
    show(RConsole);
}

static void menulazy(control m)
{
    consoletogglelazy(RConsole);
    show(RConsole);
}

static void menukill(control m)
{
    show(RConsole);
    UserBreak = 1;
}

static void menuls(control m)
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole,"ls()");
    show(RConsole);
}

static void menurm(control m)
{
    if (!ConsoleAcceptCmd) return;
    if (askyesno("Are you sure?") == YES)
	consolecmd(RConsole, "rm(list=ls())");
    show(RConsole);
}

static void menusearch(control m)
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole, "search()");
    show(RConsole);
}

static void menuconsolehelp(control m)
{
    consolehelp();
    show(RConsole);
}

static void menuhelp(control m)
{
    char *s;
    static char olds[256] = "";

    if (!ConsoleAcceptCmd) return;
    s = askstring("Help on", olds);
    show(RConsole);
    if (s) {
	sprintf(cmd, "help(\"%s\")", s);
	if (strlen(s) > 256) s[255] = '\0';
	strcpy(olds, s);
	consolecmd(RConsole, cmd);
    }
}

/*static void menumainman(control m)
{
    consolecmd(RConsole, "shell.exec('doc/manual/Manual.pdf')");
}*/

static void menumainref(control m)
{
    consolecmd(RConsole, "shell.exec('doc/manual/refman.pdf')");
}

static void menumainext(control m)
{
    consolecmd(RConsole, "shell.exec('doc/manual/R-exts.pdf')");
}

static void menuapropos(control m)
{
    char *s;
    static char olds[256] = "";

    if (!ConsoleAcceptCmd) return;
    s = askstring("Apropos", olds);
    show(RConsole);
    if (s) {
	sprintf(cmd, "apropos(\"%s\")", s);
	if (strlen(s) > 256) s[255] = '\0';
	strcpy(olds, s);
	consolecmd(RConsole, cmd);
    }
}

static void menuhelpstart(control m)
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole, "help.start()");
    show(RConsole);
}

static void menuabout(control m)
{
    char  s[256];

    sprintf(s, "%s %s.%s %s\n%s, %s\n\n%s",
	    "R", R_MAJOR, R_MINOR, "- A Language and Environment",
	    "              Copyright ", R_YEAR,
	    "    The R Development Core Team");
    askok(s);
    show(RConsole);
}

/* some menu command can be issue only if R is waiting for input */
static void menuact(control m)
{
    if (consolegetlazy(RConsole))
	check(mlazy);
    else
	uncheck(mlazy);
    if (ConsoleAcceptCmd) {
	enable(msource);
	enable(mdisplay);
	enable(mload);
	enable(msave);
	enable(mls);
	enable(mrm);
	enable(msearch);
	enable(mhelp);
	enable(mmanref);
	enable(mmanext);
	enable(mapropos);
	enable(mhelpstart);
    }
    if (consolecancopy(RConsole)) {
	enable(mcopy);
	enable(mcopypaste);
    } else {
	disable(mcopy);
	disable(mcopypaste);
    }
    if (consolecanpaste(RConsole))
	enable(mpaste);
    else
	disable(mpaste);
    if (!ConsoleAcceptCmd) {
	disable(msource);
	disable(mdisplay);
	disable(mload);
	disable(msave);
	disable(mls);
	disable(mrm);
	disable(msearch);
	disable(mhelp);
	disable(mmanref);
	disable(mmanext);
	disable(mapropos);
	disable(mhelpstart);
    }
    draw(RMenuBar);
}

#define MCHECK(m) {if(!(m)) {del(RConsole); return 0;}}

static void readconsolecfg()
{
    int   consoler, consolec, pagerrow, pagercol, multiplewin, widthonresize;
    rgb   consolebg, consolefg, consoleuser;
    int   ok, fnchanged, done, cfgerr;
    char  fn[128] = "FixedFont";
    int   sty = Plain;
    int   pointsize = 12;
    char  optf[MAX_PATH];
    char *opt[2];

    consoler = 32;
    consolec = 90;
    consolebg = White;
    consolefg = Black;
    consoleuser = Red;
    pagerrow = 25;
    pagercol = 80;
    multiplewin = 0;
    widthonresize = 1;
    if (MDIset == 1)
	RguiMDI = RguiMDI |= RW_MDI;
    if (MDIset == -1)
	RguiMDI = RguiMDI &= ~RW_MDI;

    sprintf(optf, "%s/RConsole", getenv("R_USER"));
    if (!optopenfile(optf)) {
	sprintf(optf, "%s/etc/RConsole", getenv("R_HOME"));
	if (!optopenfile(optf))
	    return;
    }
    cfgerr = 0;
    fnchanged = 0;
    while ((ok = optread(opt, '='))) {
	done = 0;
	if (ok == 2) {
	    if (!strcmp(opt[0], "font")) {
		strcpy(fn, opt[1]);
		fnchanged = 1;
		done = 1;
	    }
	    if (!strcmp(opt[0], "points")) {
		pointsize = atoi(opt[1]);
		fnchanged = 1;
		done = 1;
	    }
	    if (!strcmp(opt[0], "style")) {
		fnchanged = 1;
		if (!strcmp(opt[1], "normal")) {
		    sty = Plain;
		    done = 1;
		}
		if (!strcmp(opt[1], "bold")) {
		    sty = Bold;
		    done = 1;
		}
		if (!strcmp(opt[1], "italic")) {
		    sty = Italic;
		    done = 1;
		}
	    }
	    if (!strcmp(opt[0], "rows")) {
		consoler = atoi(opt[1]);
		done = 1;
	    }
	    if (!strcmp(opt[0], "columns")) {
		consolec = atoi(opt[1]);
		done = 1;
	    }
	    if (!strcmp(opt[0], "pgrows")) {
		pagerrow = atoi(opt[1]);
		done = 1;
	    }
	    if (!strcmp(opt[0], "pgcolumns")) {
		pagercol = atoi(opt[1]);
		done = 1;
	    }
	    if (!strcmp(opt[0], "pagerstyle")) {
		if (!strcmp(opt[1], "singlewindow"))
		    multiplewin = 0;
		else
		    multiplewin = 1;
		done = 1;
	    }
	    if (!strcmp(opt[0], "MDI")) {
		if (!MDIset && !strcmp(opt[1], "yes"))
		    RguiMDI = RguiMDI |= RW_MDI;
		else if (!MDIset && !strcmp(opt[1], "no"))
		    RguiMDI = RguiMDI &= ~RW_MDI;
		done = 1;
	    }
	    if (!strcmp(opt[0], "toolbar")) {
		if (!strcmp(opt[1], "yes"))
		    RguiMDI = RguiMDI |= RW_TOOLBAR;
		else if (!strcmp(opt[1], "no"))
		    RguiMDI = RguiMDI &= ~RW_TOOLBAR;
		done = 1;
	    }
	    if (!strcmp(opt[0], "statusbar")) {
		if (!strcmp(opt[1], "yes"))
		    RguiMDI = RguiMDI |= RW_STATUSBAR;
		else if (!strcmp(opt[1], "no"))
		    RguiMDI = RguiMDI &= ~RW_STATUSBAR;
		done = 1;
	    }
	    if (!strcmp(opt[0], "background")) {
		consolebg = nametorgb(opt[1]);
		if (consolebg != Transparent)
		    done = 1;
	    }
	    if (!strcmp(opt[0], "normaltext")) {
		consolefg = nametorgb(opt[1]);
		if (consolefg != Transparent)
		    done = 1;
	    }
	    if (!strcmp(opt[0], "usertext")) {
		consoleuser = nametorgb(opt[1]);
		if (consoleuser != Transparent)
		    done = 1;
	    }
	    if (!strcmp(opt[0], "setwidthonresize")) {
		if (!strcmp(opt[1], "yes"))
		    widthonresize = 1;
		else if (!strcmp(opt[1], "no"))
		    widthonresize = 0;
		done = 1;
	    }
	}
	if (!done) {
	    char  buf[128];

	    sprintf(buf, "Error at line %d of file %s", optline(), optfile());
	    askok(buf);
	    cfgerr = 1;
	}
    }
    if (cfgerr) {
	app_cleanup();
	exit(10);
    }
    setconsoleoptions(fn, sty, pointsize, consoler, consolec, consolefg,
		      consoleuser, consolebg,
		      pagerrow, pagercol, multiplewin, widthonresize);
}

static void closeconsole(control m)
{
    R_CleanUp(SA_DEFAULT, 0, 1);
}

void setup_term_ui()
{
    initapp(0, 0);
    readconsolecfg();
}

static MenuItem ConsolePopup[] = {
    {"Copy", menucopy, 0},
    {"Paste", menupaste, 0},
    {"Copy and paste", menucopypaste, 0},
    {"-", 0, 0},
    {"Select all", menuselectall, 0},
    {"-", 0, 0},
    {"Buffered output", menulazy, 0},
    LASTMENUITEM
};

static void popupact(control m)
{
    if (consolegetlazy(RConsole))
	check(ConsolePopup[6].m);
    else
	uncheck(ConsolePopup[6].m);

    if (consolecancopy(RConsole)) {
	enable(ConsolePopup[0].m);
	enable(ConsolePopup[2].m);
    } else {
	disable(ConsolePopup[0].m);
	disable(ConsolePopup[2].m);
    }
    if (consolecanpaste(RConsole))
	enable(ConsolePopup[1].m);
    else
	disable(ConsolePopup[1].m);
}


int setupui()
{
    initapp(0, 0);
    readconsolecfg();
    if (RguiMDI & RW_MDI) {
	TRACERUI("Rgui");
	RFrame = newwindow("RGui", rect(0, 0, 0, 0),
			   StandardWindow | Menubar | Workspace);
	setclose(RFrame, closeconsole);
	show(RFrame);
	TRACERUI("Rgui done");
    }
    TRACERUI("Console");
    if (!(RConsole = newconsole("R Console",
				StandardWindow | Document | Menubar)))
	return 0;
    TRACERUI("Console done");
    if (ismdi() && (RguiMDI & RW_TOOLBAR)) {
          int btsize = 24;
          rect r = rect(2, 2, btsize, btsize);
          control tb, bt;

          MCHECK(tb = newtoolbar(btsize + 4));
          addto(tb);

          MCHECK(bt = newtoolbutton(open_image, r, menusource));
          MCHECK(addtooltip(bt, "Source R code"));
          r.x += (btsize + 1) ;
          
          MCHECK(bt = newtoolbutton(open1_image, r, menuloadimage));
          MCHECK(addtooltip(bt, "Load image"));
          r.x += (btsize + 1) ;

          MCHECK(bt = newtoolbutton(save_image, r, menusaveimage));
          MCHECK(addtooltip(bt,  "Save image"));
          r.x += (btsize + 6);

          MCHECK(bt = newtoolbutton(copy_image, r, menucopy));
          MCHECK(addtooltip(bt, "Copy"));
          r.x += (btsize + 1);

          MCHECK(bt = newtoolbutton(paste_image, r, menupaste));
          MCHECK(addtooltip(bt, "Paste"));
          r.x += (btsize + 1);

          MCHECK(bt = newtoolbutton(copypaste_image, r, menucopypaste));
          MCHECK(addtooltip(bt, "Copy and paste"));
          r.x += (btsize + 6);

          MCHECK(bt = newtoolbutton(stop_image,r,menukill));
          MCHECK(addtooltip(bt,"Stop current computation"));
          r.x += (btsize + 6) ;

          MCHECK(bt = newtoolbutton(print_image, r, menuprint));
          MCHECK(addtooltip(bt, "Print"));
    }
    if (ismdi() && (RguiMDI & RW_STATUSBAR)) {
	char  s[256];

	TRACERUI("status bar");
	addstatusbar();
	sprintf(s, "%s %s.%s %s",
		"R", R_MAJOR, R_MINOR, "- A Language and Environment");
	addto(RConsole);
	setstatus(s);
	TRACERUI("status bar done");
    }
    addto(RConsole);
    setclose(RConsole, closeconsole);
    MCHECK(gpopup(popupact, ConsolePopup));
    MCHECK(RMenuBar = newmenubar(menuact));
    MCHECK(newmenu("File"));
    MCHECK(msource = newmenuitem("Source R code", 0, menusource));
    MCHECK(mdisplay = newmenuitem("Display file", 0, menudisplay));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(mload = newmenuitem("Load Image", 0, menuloadimage));
    MCHECK(msave = newmenuitem("Save Image", 0, menusaveimage));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(newmenuitem("Change dir", 0, menuchangedir));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(newmenuitem("Print", 0, menuprint));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(newmenuitem("Exit", 0, menuexit));

    MCHECK(newmenu("Edit"));
    MCHECK(mcopy = newmenuitem("Copy          \tCTRL+C", 0, menucopy));
    MCHECK(mpaste = newmenuitem("Paste         \tCTRL+V", 0, menupaste));
    MCHECK(mcopypaste = newmenuitem("Copy And Paste  \tCTRL+X", 0, menucopypaste));
    MCHECK(newmenuitem("Select all", 0, menuselectall));
    MCHECK(newmenu("Misc"));
    MCHECK(newmenuitem("Stop current computation           \tESC", 0, menukill));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(mlazy = newmenuitem("Buffered output\tCTRL+W", 0, menulazy));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(mls = newmenuitem("List objects", 0, menuls));
    MCHECK(mrm = newmenuitem("Remove all objects", 0, menurm));
    MCHECK(msearch = newmenuitem("List &search path", 0, menusearch));

    newmdimenu();
    MCHECK(m = newmenu("Help"));
    MCHECK(newmenuitem("Console", 0, menuconsolehelp));
    MCHECK(mhelp = newmenuitem("R language (standard)", 0, menuhelp));
    MCHECK(mhelp = newmenuitem("R language (html)", 0, menuhelpstart));

    MCHECK(newsubmenu(m, "Manuals"));
/*    MCHECK(mmanmain = newmenuitem("R Manual", 0, menumainman));*/
    MCHECK(mmanref = newmenuitem("R Reference Manual", 0, menumainref));
    MCHECK(mmanext = newmenuitem("R Extension Writer's Manual", 0, menumainext));
    addto(m);

    MCHECK(mapropos = newmenuitem("Apropos", 0, menuapropos));
    MCHECK(newmenuitem("About", 0, menuabout));
    consolesetbrk(RConsole, menukill, ESC, 0);
    readhistory(RConsole, ".Rhistory");
    show(RConsole);
    return 1;
}

extern int  CharacterMode;
int DialogSelectFile(char *buf, int len)
{
    char *fn;

    setuserfilter("All files (*.*)\0*.*\0\0");
    fn = askfilename("Select file", "");
    if (!CharacterMode)
	show(RConsole);
    if (fn)
	strncpy(buf, fn, len);
    else
	strcpy(buf, "");
    return (strlen(buf));
}
