/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1998--2005  Guido Masarotto and Brian Ripley
 *  Copyright (C) 2004--2005  The R Foundation
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
#include <config.h>
#endif
#include "win-nls.h"

#include <Defn.h>

#ifdef Win32
#define USE_MDI 1
#endif
/* R user interface based on GraphApp */
#include "Defn.h"
#undef append /* defined by graphapp/internal.h */
#include <stdio.h>
#undef DEBUG /* needed for mingw-runtime 2.0 */
/* the user menu code looks at the internal structure */
#include "graphapp/internal.h"
#include "graphapp/ga.h"
#ifdef USE_MDI
# include "graphapp/stdimg.h"
#endif
#include "console.h"
#include "rui.h"
#include "opt.h"
#include <Rversion.h>
#include "getline/getline.h"  /* for gl_load/savehistory */
#include <Startup.h>          /* for SA_DEFAULT */

#define TRACERUI(a)

extern Rboolean UserBreak;

console RConsole = NULL;
#ifdef USE_MDI
int   RguiMDI = RW_MDI | RW_TOOLBAR | RW_STATUSBAR;
int   MDIset = 0;
window RFrame;
rect MDIsize;
#endif
extern int ConsoleAcceptCmd, R_is_running;
extern Rboolean DebugMenuitem;

static menubar RMenuBar;
static popup RConsolePopup;
static menuitem msource, mdisplay, mload, msave, mloadhistory,
    msavehistory, mpaste, mpastecmds, mcopy, mcopypaste, mlazy, mconfig,
    mls, mrm, msearch, mhelp, mmanintro, mmanref, mmandata,
    mmanext, mmanlang, mmanadmin, mman0, mapropos, mhelpstart, mhelpsearch, 
    mFAQ, mrwFAQ, mpkgl, mpkgm, mpkgi, mpkgil, mpkgu, /*mpkgb, mpkgbu,*/
    mde, mCRAN, mrepos;
static int lmanintro, lmanref, lmandata, lmanlang, lmanext, lmanadmin;
static menu m, mman;
static char cmd[1024];

#include "editor.h"

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

void closeconsole(control m)  /* can also be called from editor menus */
{
    R_CleanUp(SA_DEFAULT, 0, 1);
}

static void menusource(control m)
{
    char *fn;

    if (!ConsoleAcceptCmd) return;
    setuserfilter(G_("R files (*.R)\0*.R\0S files (*.q)\0*.q\0All files (*.*)\0*.*\0\0"));
    fn = askfilename(G_("Select file to source"), "");
    Rwin_fpset();
/*    show(RConsole); */
    if (fn) {
	fixslash(fn);
	snprintf(cmd, 1024, "source(\"%s\")", fn);
	consolecmd(RConsole, cmd);
    }
}

static void menudisplay(control m)
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole,"local({fn<-choose.files(filters=Filters[c('R','txt','All'),],index=4)\nfile.show(fn,header=fn,title='')})");
}

static void menuloadimage(control m)
{
    char *fn;

    if (!ConsoleAcceptCmd) return;
    setuserfilter(G_("R images (*.RData)\0*.RData\0R images - old extension (*.rda)\0*.rda\0All files (*.*)\0*.*\0\0"));
    fn = askfilename(G_("Select image to load"), "");
    Rwin_fpset();
/*    show(RConsole); */
    if (fn) {
	fixslash(fn);
	snprintf(cmd, 1024, "load(\"%s\")", fn);
	consolecmd(RConsole, cmd);
    }
}

static void menusaveimage(control m)
{
    char *fn;

    if (!ConsoleAcceptCmd) return;
    setuserfilter(G_("R images (*.RData)\0*.RData\0All files (*.*)\0*.*\0\0"));
    fn = askfilesave(G_("Save image in"), ".RData");
    Rwin_fpset();
/*    show(RConsole); */
    if (fn) {
	fixslash(fn);
	snprintf(cmd, 1024, "save.image(\"%s\")", fn);
	consolecmd(RConsole, cmd);
    }
}

static void menuloadhistory(control m)
{
    char *fn;

    setuserfilter(G_("All files (*.*)\0*.*\0\0"));
    fn = askfilename(G_("Load history from"), R_HistoryFile);
    Rwin_fpset();
/*    show(RConsole); */
    if (fn) {
	fixslash(fn);
	gl_loadhistory(fn);
    }
}

static void menusavehistory(control m)
{
    char *fn;

    setuserfilter(G_("All files (*.*)\0*.*\0\0"));
    fn = askfilesave(G_("Save history in"), R_HistoryFile);
    Rwin_fpset();
/*    show(RConsole); */
    if (fn) {
	fixslash(fn);
	R_setupHistory(); /* re-read the history size */
	gl_savehistory(fn, R_HistorySize);
    }
}

static void menuchangedir(control m)
{
    askchangedir();
    Rwin_fpset();
/*    show(RConsole); */
}

static void menuprint(control m)
{
    consoleprint(RConsole);
/*    show(RConsole); */
}

static void menusavefile(control m)
{
    consolesavefile(RConsole, 0);
/*    show(RConsole); */
}

static void menuexit(control m)
{
    closeconsole(m);
}

static void menuselectall(control m)
{
    consoleselectall(RConsole);
/*    show(RConsole); */
}

static void menucopy(control m)
{
    if (consolecancopy(RConsole))
	consolecopy(RConsole);
    else
	askok(G_("No selection"));
/*    show(RConsole); */
}

static void menupaste(control m)
{
    if (consolecanpaste(RConsole))
	consolepaste(RConsole);
    else
	askok(G_("No text available"));
/*    show(RConsole); */
}

static void menupastecmds(control m)
{
    if (consolecanpaste(RConsole))
	consolepastecmds(RConsole);
    else
	askok(G_("No text available"));
}

static void menucopypaste(control m)
{
    if (consolecancopy(RConsole)) {
	consolecopy(RConsole);
	consolepaste(RConsole);
    } else
	askok(G_("No selection"));
/*    show(RConsole); */
}

/* button* versions force focus back to the console: needed for PR#3285 */
static void buttoncopy(control m)
{
    menucopy(m);
    show(RConsole);
}

static void buttonpaste(control m)
{
    menupaste(m);
    show(RConsole);
}

static void buttoncopypaste(control m)
{
    menucopypaste(m);
    show(RConsole);
}

static void buttonkill(control m)
{
    show(RConsole);
    UserBreak = TRUE;
}

void menuclear(control m)
{
    consoleclear(RConsole);
}

static void menude(control m)
{
    char *s;
    SEXP var;

    if (!ConsoleAcceptCmd) return;
    s = askstring(G_("Name of data frame or matrix"), "");
    if(s) {
	var = findVar(install(s), R_GlobalEnv);
	if (var != R_UnboundValue) {
	    snprintf(cmd, 1024,"fix(%s)", s);
	    consolecmd(RConsole, cmd);
	} else {
	    snprintf(cmd, 1024, G_("'%s' cannot be found"), s);
	    askok(cmd);
	}
    }
/*    show(RConsole); */
}

void menuconfig(control m)
{
    Rgui_configure();
/*    show(RConsole); */
}

static void menulazy(control m)
{
    consoletogglelazy(RConsole);
/*    show(RConsole); */
}

static void menuconsolestayontop(control m)
{
    BringToTop(RConsole, 2);
}

static void menukill(control m)
{
    /*  show(RConsole); */
    UserBreak = TRUE;
}

static Rboolean isdebuggerpresent()
{
    typedef BOOL (*R_CheckDebugger)();
    R_CheckDebugger entry;
    entry = (R_CheckDebugger)GetProcAddress((HMODULE)GetModuleHandle("KERNEL32"),
                                            "IsDebuggerPresent");
    if (entry == NULL) return(FALSE);
    else return((Rboolean)entry());
}

void breaktodebugger()
{
    asm("int $3");
}

static void menudebug(control m)
{
    breaktodebugger();
}

static void menuls(control m)
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole,"ls()");
/*    show(RConsole); */
}

static void menurm(control m)
{
    if (!ConsoleAcceptCmd) return;
    if (askyesno(G_("Are you sure?")) == YES)
	consolecmd(RConsole, "rm(list=ls(all=TRUE))");
/*    show(RConsole); */
}

static void menusearch(control m)
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole, "search()");
/*    show(RConsole); */
}

static void menupkgload(control m)
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole,
	       "local({pkg <- select.list(sort(.packages(all.available = TRUE)))\nif(nchar(pkg)) library(pkg, character.only=TRUE)})");
/*    show(RConsole); */
}

static void menupkgupdate(control m)
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole, "update.packages(ask='graphics')");
/*    show(RConsole); */
}

#if 0
static void menupkgupdatebioc(control m)
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole,
	       "update.packages(repos=getOption(\"BIOC\"))");
/*    show(RConsole); */
}


static void menupkginstallbioc(control m) 
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole, "utils:::menuInstallBioc()");
}
#endif

static void menupkgcranmirror(control m)
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole, "utils:::chooseCRANmirror()");
}

static void menupkgrepos(control m)
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole, "utils::setRepositories()");
}

static void menupkginstallpkgs(control m)
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole, "utils:::menuInstallPkgs()");
/*    show(RConsole); */
}

static void menupkginstalllocal(control m)
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole, "utils:::menuInstallLocal()");
}

static void menuconsolehelp(control m)
{
    consolehelp();
/*    show(RConsole); */
}

static void menuhelp(control m)
{
    char *s;
    static char olds[256] = "";

    if (!ConsoleAcceptCmd) return;
    s = askstring(G_("Help on"), olds);
/*    show(RConsole); */
    if (s) {
	snprintf(cmd, 1024, "help(\"%s\")", s);
	if (strlen(s) > 255) s[255] = '\0';
	strcpy(olds, s);
	consolecmd(RConsole, cmd);
    }
}

static void menumainman(control m)
{
    internal_shellexec("doc\\manual\\R-intro.pdf");
}

static void menumainref(control m)
{
    internal_shellexec("doc\\manual\\refman.pdf");
}

static void menumaindata(control m)
{
    internal_shellexec("doc\\manual\\R-data.pdf");
}

static void menumainext(control m)
{
    internal_shellexec("doc\\manual\\R-exts.pdf");
}

static void menumainlang(control m)
{
    internal_shellexec("doc\\manual\\R-lang.pdf");
}

static void menumainadmin(control m)
{
    internal_shellexec("doc\\manual\\R-admin.pdf");
}

static void menuhelpsearch(control m)
{
    char *s;
    static char olds[256] = "";

    if (!ConsoleAcceptCmd) return;
    s = askstring(G_("Search help"), olds);
    if (s && strlen(s)) {
	snprintf(cmd, 1024, "help.search(\"%s\")", s);
	if (strlen(s) > 255) s[255] = '\0';
	strcpy(olds, s);
	consolecmd(RConsole, cmd);
    }
}

static void menuapropos(control m)
{
    char *s;
    static char olds[256] = "";

    if (!ConsoleAcceptCmd) return;
    s = askstring(G_("Apropos"), olds);
/*    show(RConsole); */
    if (s) {
	snprintf(cmd, 1024, "apropos(\"%s\")", s);
	if (strlen(s) > 255) s[255] = '\0';
	strcpy(olds, s);
	consolecmd(RConsole, cmd);
    }
}

static void menuhelpstart(control m)
{
/*    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole, "help.start()");
    show(RConsole);*/
    internal_shellexec("doc\\html\\rwin.html");
}

static void menuFAQ(control m)
{
    internal_shellexec("doc\\manual\\R-FAQ.html");
}

static void menurwFAQ(control m)
{
    internal_shellexec("doc\\html\\rw-FAQ.html");
}

static void menuabout(control m)
{
    char  s[256];

    sprintf(s, "%s %s.%s %s\n%s, %s\n\n%s",
	    "R", R_MAJOR, R_MINOR, "- A Language and Environment",
	    "              Copyright ", R_YEAR,
	    "    The R Development Core Team");
    askok(s);
/*    show(RConsole); */
}

static void menuRhome(control m)
{
    ShellExecute(NULL, "open", "http://www.r-project.org", NULL, NULL, SW_SHOW);
}

static void menuCRAN(control m)
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole, "utils:::menuShowCRAN()");
}


/* some menu commands can be issued only if R is waiting for input */
static void menuact(control m)
{
    if (consolegetlazy(RConsole)) check(mlazy); else uncheck(mlazy);

    /* display needs pager set */
    if (R_is_running) enable(mdisplay); else disable(mdisplay);

    if (ConsoleAcceptCmd) {
	enable(msource);
	enable(mload);
	enable(msave);
	enable(mls);
	enable(mrm);
	enable(msearch);
	enable(mhelp);
	enable(mhelpsearch);
	enable(mapropos);
	enable(mpkgl);
	enable(mpkgm);
	enable(mpkgi);
	enable(mpkgil);
	enable(mpkgu);
	/* enable(mpkgb); enable(mpkgbu); */
	enable(mde);
	enable(mCRAN);
	enable(mrepos);
    } else {
	disable(msource);
	disable(mload);
	disable(msave);
	disable(mls);
	disable(mrm);
	disable(msearch);
	disable(mhelp);
	disable(mhelpsearch);
	disable(mapropos);
	disable(mpkgl);
	disable(mpkgm);
	disable(mpkgi);
	disable(mpkgil);
	disable(mpkgu);
	/* disable(mpkgb); disable(mpkgbu); */
	disable(mde);
	disable(mCRAN);
	disable(mrepos);
    }

    if (consolecancopy(RConsole)) {
	enable(mcopy);
	enable(mcopypaste);
    } else {
	disable(mcopy);
	disable(mcopypaste);
    }

    if (consolecanpaste(RConsole)) {
	enable(mpaste);
	enable(mpastecmds);
    }
    else {
	disable(mpaste);
	disable(mpastecmds);
    }

    draw(RMenuBar);
}

#define MCHECK(m) {if(!(m)) {del(RConsole); return 0;}}

/* This file will always be ASCII */
void readconsolecfg()
{
    int   consoler, consolec, consolex, consoley, pagerrow, pagercol,
	multiplewin, widthonresize;
    int   bufbytes, buflines;
    rgb   consolebg, consolefg, consoleuser, highlight ;
    int   ok, fnchanged, done, cfgerr;
    char  fn[128] = "FixedFont";
    int   sty = Plain;
    int   pointsize = 12;
    char  optf[PATH_MAX];
    char *opt[2];

    consoler = 32;
    consolec = 90;
    consolex = consoley = 0;
    consolebg = White;
    consolefg = Black;
    consoleuser = gaRed;
    highlight = DarkRed;
    pagerrow = 25;
    pagercol = 80;
    multiplewin = 0;
    bufbytes = 64*1024;
    buflines = 8*1024;
    widthonresize = 1;
#ifdef USE_MDI
    if (MDIset == 1)
	RguiMDI |= RW_MDI;
    if (MDIset == -1)
	RguiMDI &= ~RW_MDI;
    MDIsize = rect(0, 0, 0, 0);
#endif
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
		if(strlen(opt[1]) > 127) opt[1][127] = '\0';
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
	    if (!strcmp(opt[0], "xconsole")) {
		consolex = atoi(opt[1]);
		done = 1;
	    }
	    if (!strcmp(opt[0], "yconsole")) {
		consoley = atoi(opt[1]);
		done = 1;
	    }
	    if (!strcmp(opt[0], "xgraphics")) {
		Rwin_graphicsx = atoi(opt[1]);
		done = 1;
	    }
	    if (!strcmp(opt[0], "ygraphics")) {
		Rwin_graphicsy = atoi(opt[1]);
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
	    if (!strcmp(opt[0], "bufbytes")) {
		bufbytes = atoi(opt[1]);
		done = 1;
	    }
	    if (!strcmp(opt[0], "buflines")) {
		buflines = atoi(opt[1]);
		done = 1;
	    }
#ifdef USE_MDI
	    if (!strcmp(opt[0], "MDI")) {
		if (!MDIset && !strcmp(opt[1], "yes"))
		    RguiMDI |= RW_MDI;
		else if (!MDIset && !strcmp(opt[1], "no"))
		    RguiMDI &= ~RW_MDI;
		done = 1;
	    }
	    if (!strcmp(opt[0], "toolbar")) {
		if (!strcmp(opt[1], "yes"))
		    RguiMDI |= RW_TOOLBAR;
		else if (!strcmp(opt[1], "no"))
		    RguiMDI &= ~RW_TOOLBAR;
		done = 1;
	    }
	    if (!strcmp(opt[0], "statusbar")) {
		if (!strcmp(opt[1], "yes"))
		    RguiMDI |= RW_STATUSBAR;
		else if (!strcmp(opt[1], "no"))
		    RguiMDI &= ~RW_STATUSBAR;
		done = 1;
	    }
	    if (!strcmp(opt[0], "MDIsize")) { /* wxh+x+y */
		int x=0, y=0, w=0, h=0, sign;
		char *p = opt[1];

		if(*p == '-') {sign = -1; p++;} else sign = +1;
		for(w=0; isdigit(*p); p++) w = 10*w + (*p - '0');
		w *= sign;
		p++;

		if(*p == '-') {sign = -1; p++;} else sign = +1;
		for(h=0; isdigit(*p); p++) h = 10*h + (*p - '0');
		h *= sign;

		if(*p == '-') sign = -1; else sign = +1;
		p++;
		for(x=0; isdigit(*p); p++) x = 10*x + (*p - '0');
		x *= sign;
		if(*p == '-') sign = -1; else sign = +1;
		p++;
		for(y=0; isdigit(*p); p++) y = 10*y + (*p - '0');
		y *= sign;

		MDIsize = rect(x, y, w, h);
		done = 1;
	    }
#endif
	    if (!strcmp(opt[0], "background")) {
		if (!strcmpi(opt[1], "Windows"))
		    consolebg = myGetSysColor(COLOR_WINDOW);
		else consolebg = nametorgb(opt[1]);
		if (consolebg != Transparent)
		    done = 1;
	    }
	    if (!strcmp(opt[0], "normaltext")) {
		if (!strcmpi(opt[1], "Windows"))
		    consolefg = myGetSysColor(COLOR_WINDOWTEXT);
		else consolefg = nametorgb(opt[1]);
		if (consolefg != Transparent)
		    done = 1;
	    }
	    if (!strcmp(opt[0], "usertext")) {
		if (!strcmpi(opt[1], "Windows"))
		    consoleuser = myGetSysColor(COLOR_ACTIVECAPTION);
		else consoleuser = nametorgb(opt[1]);
		if (consoleuser != Transparent)
		    done = 1;
	    }
	    if (!strcmp(opt[0], "highlight")) {
		if (!strcmpi(opt[1], "Windows"))
		    highlight = myGetSysColor(COLOR_ACTIVECAPTION);
		else highlight = nametorgb(opt[1]);
		if (highlight != Transparent)
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

	    snprintf(buf, 128, G_("Error at line %d of file %s"),
		     optline(), optfile());
	    askok(buf);
	    cfgerr = 1;
	}
    }
    if (cfgerr) {
	app_cleanup();
	RConsole = NULL;
	exit(10);
    }
    setconsoleoptions(fn, sty, pointsize, consoler, consolec,
		      consolex, consoley,
		      consolefg, consoleuser, consolebg, highlight,
		      pagerrow, pagercol, multiplewin, widthonresize,
		      bufbytes, buflines);
}

static void dropconsole(control m, char *fn)
{
    char *p;

    p = Rf_strrchr(fn, '.');
    if(p) {
	/* OK even in MBCS */
	if(stricmp(p+1, "R") == 0) {
	    if(ConsoleAcceptCmd) {
		R_fixslash(fn);
		snprintf(cmd, 1024, "source(\"%s\")", fn);
		consolecmd(RConsole, cmd);
	    }
	/* OK even in MBCS */
	} else if(stricmp(p+1, "RData") == 0 || stricmp(p+1, "rda")) {
	    if(ConsoleAcceptCmd) {
		R_fixslash(fn);
		snprintf(cmd, 1024, "load(\"%s\")", fn);
		consolecmd(RConsole, cmd);
	    }
	}
	return;
    }
    askok(G_("Can only drag-and-drop .R, .RData and .rda files"));
}

static MenuItem ConsolePopup[] = {	  /* Numbers used below */
    {GN_("Copy"), menucopy, 'C', 0},			  /* 0 */
    {GN_("Paste"), menupaste, 'V', 0},		  /* 1 */
    {GN_("Paste commands only"), menupastecmds, 0, 0},  /* 2 */
    {GN_("Copy and paste"), menucopypaste, 'X', 0},	  /* 3 */
    {"-", 0, 0, 0},
    {GN_("Clear window"), menuclear, 'L', 0},          /* 5 */
    {"-", 0, 0, 0},
    {GN_("Select all"), menuselectall, 0, 0},	  /* 7 */
    {"-", 0, 0},
    {GN_("Buffered output"), menulazy, 'W', 0},	  /* 9 */
    {GN_("Stay on top"), menuconsolestayontop, 0, 0},  /* 10 */
    LASTMENUITEM
};

static void popupact(control m)
{
    if (consolegetlazy(RConsole))
	check(ConsolePopup[9].m);
    else
	uncheck(ConsolePopup[9].m);

    if (consolecancopy(RConsole)) {
	enable(ConsolePopup[0].m);
	enable(ConsolePopup[3].m);
    } else {
	disable(ConsolePopup[0].m);
	disable(ConsolePopup[3].m);
    }
    if (consolecanpaste(RConsole)) {
	enable(ConsolePopup[1].m);
	enable(ConsolePopup[2].m);
    } else {
	disable(ConsolePopup[1].m);
	disable(ConsolePopup[2].m);
    }
    if (ismdi())
    	disable(ConsolePopup[10].m);
    else {
	if (isTopmost(RConsole))
	    check(ConsolePopup[10].m);
	else
	    uncheck(ConsolePopup[10].m);
    }
}

/* Package management menu is common to all R windows */

int RguiPackageMenu()
{
    MCHECK(newmenu(G_("Packages")));
    MCHECK(mpkgl = newmenuitem(G_("Load package..."), 0, menupkgload));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(mpkgm = newmenuitem(G_("Set CRAN mirror..."), 0,
			       menupkgcranmirror));
    MCHECK(mrepos = newmenuitem(G_("Select repositories..."), 0,
				menupkgrepos));
    MCHECK(mpkgi = newmenuitem(G_("Install package(s)..."), 0,
			       menupkginstallpkgs));
    MCHECK(mpkgu = newmenuitem(G_("Update packages"), 0,
			       menupkgupdate));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(mpkgil = newmenuitem(G_("Install package(s) from local zip files..."),
				0, menupkginstalllocal));
/*    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(mpkgb = newmenuitem(G_("Install package(s) from Bioconductor..."),
			       0, menupkginstallbioc));
    MCHECK(mpkgbu = newmenuitem(G_("Update packages from Bioconductor"),
    0, menupkgupdatebioc)); */
    return 0;
}

/* Help functions common to all R windows. 
   These should be appended to each context-specific help menu */

int RguiCommonHelp(menu m)
{
    addto(m);

    MCHECK(mFAQ = newmenuitem(G_("FAQ on R"), 0, menuFAQ));
    if (!check_doc_file("doc\\manual\\R-FAQ.html")) disable(mFAQ);
    MCHECK(mrwFAQ = newmenuitem(G_("FAQ on R for &Windows"), 0, menurwFAQ));
    if (!check_doc_file("doc\\html\\rw-FAQ.html")) disable(mrwFAQ);

    lmanintro = check_doc_file("doc\\manual\\R-intro.pdf");
    lmanref = check_doc_file("doc\\manual\\refman.pdf");
    lmandata = check_doc_file("doc\\manual\\R-data.pdf");
    lmanlang = check_doc_file("doc\\manual\\R-lang.pdf");
    lmanext = check_doc_file("doc\\manual\\R-exts.pdf");
    lmanadmin = check_doc_file("doc\\manual\\R-admin.pdf");
    if (!lmanintro && !lmanref && !lmandata && !lmanlang && !lmanext 
       && !lmanadmin) {
	MCHECK(mman0 = newmenuitem(G_("Manuals (in PDF)"), 0, NULL));
	disable(mman0);
    } else {
	MCHECK(mman = newsubmenu(m, G_("Manuals (in PDF)")));
	MCHECK(mmanintro = newmenuitem(G_("An &Introduction to R"), 0, 
				       menumainman));
	if (!lmanintro) disable(mmanintro);
	MCHECK(mmanref = newmenuitem(G_("R &Reference Manual"), 0, 
				     menumainref));
	if (!lmanref) disable(mmanref);
	MCHECK(mmandata = newmenuitem(G_("R Data Import/Export"), 0, 
				      menumaindata));
	if (!lmandata) disable(mmandata);
	MCHECK(mmanlang = newmenuitem(G_("R Language Definition"), 0, 
				      menumainlang));
	if (!lmanlang) disable(mmanlang);
	MCHECK(mmanext = newmenuitem(G_("Writing R Extensions"), 0, 
				     menumainext));
	if (!lmanext) disable(mmanext);
	MCHECK(mmanadmin = newmenuitem(G_("R Installation and Administration"), 0, 
				     menumainadmin));	
	if (!lmanadmin) disable(mmanadmin);
    }
    

    addto(m);
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(mhelp = newmenuitem(G_("R functions (text)..."), 0, menuhelp));
    MCHECK(mhelpstart = newmenuitem(G_("Html help"), 0, menuhelpstart));
    if (!check_doc_file("doc\\html\\rwin.html")) disable(mhelpstart);
    MCHECK(mhelpsearch = newmenuitem(G_("Search help..."), 0, menuhelpsearch));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(mapropos = newmenuitem(G_("Apropos..."), 0, menuapropos));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(newmenuitem(G_("R Project home page"), 0, menuRhome));
    MCHECK(mCRAN = newmenuitem(G_("CRAN home page"), 0, menuCRAN));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(newmenuitem(G_("About"), 0, menuabout));
    return 0;
}


int setupui()
{
    initapp(0, 0);
    readconsolecfg();
#ifdef USE_MDI
    if (RguiMDI & RW_MDI) {
	TRACERUI("Rgui");
	RFrame = newwindow("RGui", MDIsize,
			   StandardWindow | Menubar | Workspace);
	setclose(RFrame, closeconsole);
	show(RFrame);
	TRACERUI("Rgui done");
    }
#endif
    TRACERUI("Console");
    if (!(RConsole = newconsole("R Console",
				StandardWindow | Document | Menubar)))
	return 0;
    TRACERUI("Console done");
#ifdef USE_MDI
    if (ismdi() && (RguiMDI & RW_TOOLBAR)) {
          int btsize = 24;
          rect r = rect(2, 2, btsize, btsize);
          control tb, bt;

          MCHECK(tb = newtoolbar(btsize + 4));
          addto(tb);

          MCHECK(bt = newtoolbutton(open_image, r, menueditoropen));
          MCHECK(addtooltip(bt, G_("Open script")));
          r.x += (btsize + 1) ;

          MCHECK(bt = newtoolbutton(open1_image, r, menuloadimage));
          MCHECK(addtooltip(bt, G_("Load image")));
          r.x += (btsize + 1) ;

          MCHECK(bt = newtoolbutton(save_image, r, menusaveimage));
          MCHECK(addtooltip(bt, G_("Save image")));
          r.x += (btsize + 6);

          MCHECK(bt = newtoolbutton(copy_image, r, buttoncopy));
          MCHECK(addtooltip(bt, G_("Copy")));
          r.x += (btsize + 1);

          MCHECK(bt = newtoolbutton(paste_image, r, buttonpaste));
          MCHECK(addtooltip(bt, G_("Paste")));
          r.x += (btsize + 1);

          MCHECK(bt = newtoolbutton(copypaste_image, r, buttoncopypaste));
          MCHECK(addtooltip(bt, G_("Copy and paste")));
          r.x += (btsize + 6);

          MCHECK(bt = newtoolbutton(stop_image, r, buttonkill));
          MCHECK(addtooltip(bt, G_("Stop current computation")));
          r.x += (btsize + 6) ;

          MCHECK(bt = newtoolbutton(print_image, r, menuprint));
          MCHECK(addtooltip(bt, G_("Print")));
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
#endif
    addto(RConsole);
    setclose(RConsole, closeconsole);
    setdrop(RConsole, dropconsole);
    MCHECK(RConsolePopup = gpopup(popupact, ConsolePopup));
    MCHECK(RMenuBar = newmenubar(menuact));
    MCHECK(newmenu(G_("File")));
    MCHECK(msource = newmenuitem(G_("Source R code..."), 0, menusource));
    MCHECK(newmenuitem(G_("New script"), 0, menueditornew));
    MCHECK(newmenuitem(G_("Open script..."), 0, menueditoropen));
    MCHECK(mdisplay = newmenuitem(G_("Display file(s)..."), 0, menudisplay));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(mload = newmenuitem(G_("Load Workspace..."), 0, menuloadimage));
    MCHECK(msave = newmenuitem(G_("Save Workspace..."), 0, menusaveimage));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(mloadhistory = newmenuitem(G_("Load History..."), 0,
				      menuloadhistory));
    MCHECK(msavehistory = newmenuitem(G_("Save History..."), 0,
				      menusavehistory));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(newmenuitem(G_("Change dir..."), 0, menuchangedir));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(newmenuitem(G_("Print..."), 0, menuprint));
    MCHECK(newmenuitem(G_("Save to File..."), 0, menusavefile));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(newmenuitem(G_("Exit"), 0, menuexit));

    MCHECK(newmenu(G_("Edit")));
    MCHECK(mcopy = newmenuitem(G_("Copy"), 'C', menucopy));
    MCHECK(mpaste = newmenuitem(G_("Paste"), 'V', menupaste));
    MCHECK(mpastecmds = newmenuitem(G_("Paste commands only"), 0, 
				    menupastecmds));
    MCHECK(mcopypaste = newmenuitem(G_("Copy and Paste"), 'X', menucopypaste));
    MCHECK(newmenuitem(G_("Select all"), 0, menuselectall));
    MCHECK(newmenuitem(G_("Clear console"), 'L', menuclear));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(mde = newmenuitem(G_("Data editor..."), 0, menude));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(mconfig = newmenuitem(G_("GUI preferences..."), 0, menuconfig));

    MCHECK(newmenu(G_("Misc")));
    MCHECK(newmenuitem(G_("Stop current computation           \tESC"), 0, 
		       menukill));
    if (DebugMenuitem || isdebuggerpresent())
	MCHECK(newmenuitem(G_("Break to debugger"), 0, menudebug));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(mlazy = newmenuitem(G_("Buffered output"), 'W', menulazy));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(mls = newmenuitem(G_("List objects"), 0, menuls));
    MCHECK(mrm = newmenuitem(G_("Remove all objects"), 0, menurm));
    MCHECK(msearch = newmenuitem(G_("List &search path"), 0, menusearch));

    RguiPackageMenu();
#ifdef USE_MDI
    newmdimenu();
#endif
    MCHECK(m = newmenu(G_("Help")));
    MCHECK(newmenuitem(G_("Console"), 0, menuconsolehelp));
    MCHECK(newmenuitem("-", 0, NULL));
    RguiCommonHelp(m);
    consolesetbrk(RConsole, menukill, ESC, 0);
    gl_hist_init(R_HistorySize, 0);
    if (R_RestoreHistory) gl_loadhistory(R_HistoryFile);
    show(RConsole);
    return 1;
}

#ifdef USE_MDI
static RECT RframeRect; /* for use by pagercreate */
RECT *RgetMDIsize()
{
    GetClientRect(hwndClient, &RframeRect);
    return &RframeRect;
}

int RgetMDIwidth()
{
    return RgetMDIsize()->right;
}

int RgetMDIheight()
{
    return RgetMDIsize()->bottom;
}
#endif

extern int  CharacterMode;
int DialogSelectFile(char *buf, int len)
{
    char *fn;

    setuserfilter(G_("All files (*.*)\0*.*\0\0"));
    fn = askfilename(G_("Select file"), "");
    Rwin_fpset();
/*    if (!CharacterMode)
  	show(RConsole); */
    if (fn)
	strncpy(buf, fn, len);
    else
	strcpy(buf, "");
    return (strlen(buf));
}

static menu usermenus[10];
static char usermenunames[10][51];

static Uitem  umitems[500];

static int nmenus=0, nitems=0;

static void menuuser(control m)
{
    int item = m->max;
    char *p = umitems[item]->action;

    if (strcmp(p, "none") == 0) return;
    Rconsolecmd(p);
}

int numwinmenus(void) {
    return(nmenus);
}

char *getusermenuname(int pos) {
    return(usermenunames[pos]);
}

menuItems *wingetmenuitems(char *mname, char *errmsg) {
    menuItems *items;
    char mitem[102], *p, *q, *r;
    int i,j=0;

    q = (char *)malloc(100 * sizeof(char));
    r = (char *)malloc(100 * sizeof(char));

    items = (menuItems *)malloc(sizeof(menuItems));
    items->mItems = (Uitem *)malloc(500 * sizeof(Uitem));

    if (strlen(mname) > 100) {
	strcpy(errmsg, G_("'mname' is limited to 100 chars"));
	free(items->mItems);
	free(items);
	return NULL;
    }

    strcpy(mitem, mname); strcat(mitem, "/");

    for (i = 0; i < nitems; i++) {
	p = (char *)strstr(umitems[i]->name, mitem);

	if (p == NULL)
	    continue;
	/* the 'mitem' pattern might be showing up */
	/* as a substring in another valid name.  Make sure */
	/* this isn't the case */
	if (strlen(p) != strlen(umitems[i]->name))
	    continue;

	strcpy(q, p+strlen(mitem));
	/* Due to the way menu items are stored, it can't be */
	/* determined if this is say item 'foo' from menu 'Blah/bar' */
	/* or item 'bar/foo' from menu 'Blah'.  Check this manually */
	/* by adding the item label to the menu we're looking for. */
	sprintf(r, "%s%s", mitem, umitems[i]->m->text);
	if (strcmp(r, p) != 0)
	    continue;

	items->mItems[j] = (Uitem)malloc(sizeof(uitem));
	items->mItems[j]->name = (char *)malloc((strlen(q) + 1) * sizeof(char));
	items->mItems[j]->action = (char *)malloc((strlen(umitems[i]->action) + 1) * sizeof(char));

	strcpy(items->mItems[j]->name, q);
	strcpy(items->mItems[j]->action, umitems[i]->action);
	j++;
    }
    free(q);
    free(r);

    items->numItems = j;
    if (j == 0) sprintf(errmsg, G_("menu %s does not exist"), mname);

    return(items);
}

void freemenuitems(menuItems *items) {
    int j;

    for (j = 0; j < items->numItems; j++) {
	free(items->mItems[j]->name);
	free(items->mItems[j]->action);
	free(items->mItems[j]);
    }
    free(items->mItems);
    free(items);
}

extern menu getGraphMenu(char *); /* from devga.c */

static menu getMenu(char * name)
{
    int i;
    for (i = 0; i < nmenus; i++)
	if (strcmp(name, usermenunames[i]) == 0) return(usermenus[i]);
    if (strcmp(name, "$ConsolePopup") == 0)
	return(RConsolePopup);
    else if (strcmp(name, "$ConsoleMain") == 0)
	return(RMenuBar);
    else if (strncmp(name, "$Graph", 6) == 0)
	return(getGraphMenu(name));
    else return(NULL);
}

int winaddmenu(char * name, char *errmsg)
{
    char *p, *submenu = name, start[50];
    menu parent;

    if (getMenu(name))
    	return 0;	/* Don't add repeats */

    if (nmenus > 15) {
	strcpy(errmsg, G_("Only 16 menus are allowed"));
	return 2;
    }
    if (strlen(name) > 50) {
	strcpy(errmsg, G_("'menu' is limited to 50 chars"));
	return 5;
    }
    p = Rf_strrchr(name, '/');
    if (p) {
	submenu = p + 1;
	strcpy(start, name);
	*Rf_strrchr(start, '/') = '\0';
	parent = getMenu(start);
	if (!parent) {
	    strcpy(errmsg, G_("base menu does not exist"));
	    return 3;
	}
	m = newsubmenu(parent, submenu);
    } else {
	addto(RMenuBar);
	m = newmenu(submenu);
    }
    if (m) {
	usermenus[nmenus] = m;
	strcpy(usermenunames[nmenus], name);
	nmenus++;
	show(RConsole);
	return 0;
    } else {
	strcpy(errmsg, G_("failed to allocate menu"));
	return 1;
    }
}

int winaddmenuitem(char * item, char * menu, char * action, char *errmsg)
{
    int i, im;
    menuitem m;
    char mitem[102], *p;

    if (nitems > 499) {
	strcpy(errmsg, G_("too many menu items have been created"));
	return 2;
    }
    if (strlen(item) + strlen(menu) > 100) {
	strcpy(errmsg, G_("menu + item is limited to 100 chars"));
	return 5;
    }

    for (im = 0; im < nmenus; im++) {
	if (strcmp(menu, usermenunames[im]) == 0) break;
    }
    if (im == nmenus) {
	strcpy(errmsg, G_("menu does not exist"));
	return 3;
    }

    strcpy(mitem, menu); strcat(mitem, "/"); strcat(mitem, item);

    for (i = 0; i < nitems; i++) {
	if (strcmp(mitem, umitems[i]->name) == 0) break;
    }
    if (i < nitems) { /* existing item */
	if (strcmp(action, "enable") == 0) {
	    enable(umitems[i]->m);
	} else if (strcmp(action, "disable") == 0) {
	    disable(umitems[i]->m);
	} else {
	    p = umitems[i]->action;
	    p = realloc(p, strlen(action) + 1);
	    if(!p) {
		strcpy(errmsg, G_("failed to allocate char storage"));
		return 4;
	    }
	    strcpy(p, action);
	}
    } else {
	addto(usermenus[im]);
	m  = newmenuitem(item, 0, menuuser);
	if (m) {
	    umitems[nitems] = (Uitem) malloc(sizeof(uitem));
	    umitems[nitems]->m = m;
	    umitems[nitems]->name = p = (char *) malloc(strlen(mitem) + 1);
	    if(!p) {
		strcpy(errmsg, G_("failed to allocate char storage"));
		return 4;
	    }
	    strcpy(p, mitem);
	    if(!p) {
		strcpy(errmsg, G_("failed to allocate char storage"));
		return 4;
	    }
	    umitems[nitems]->action = p = (char *) malloc(strlen(action) + 1);
	    strcpy(p, action);
	    m->max = nitems;
	    nitems++;
	} else {
	    strcpy(errmsg, G_("failed to allocate menuitem"));
	    return 1;
	}
    }
    show(RConsole);
    return 0;
}

int windelmenu(char * menu, char *errmsg)
{
    int i, j, count = 0, len = strlen(menu);

    j = 0;
    for (i = 0; i < nmenus; i++) {
	if (strcmp(menu, usermenunames[i]) == 0
	  || (strncmp(menu, usermenunames[i], len) && usermenunames[i][len] == '/')) {
	    remove_menu_item(usermenus[i]);
	    count++;
	} else {
	    if (j < i) {
		strcpy(usermenunames[j], usermenunames[i]);
		usermenus[j] = usermenus[i];
	    }
	    j++;
        }
    }
    nmenus -= count;
    if (!count) {
	strcpy(errmsg, G_("menu does not exist"));
	return 3;
    }

    /* Delete any menu items in this menu */

    for (j = nitems-1; j >= 0; j--) {
	if (strncmp(menu, umitems[j]->name, len) == 0 && umitems[j]->name[len] == '/')
	    windelmenuitem(umitems[j]->name + len + 1, menu, errmsg);
    }

    show(RConsole);
    return 0;
}

void windelmenus(char * prefix)
{
    int i, len = strlen(prefix);

    for (i = nmenus-1; i >=0; i--) {
	if (strncmp(prefix, usermenunames[i], len) == 0)
	    windelmenu(usermenunames[i], G_("menu not found"));
    }
}

int windelmenuitem(char * item, char * menu, char *errmsg)
{
    int i;
    char mitem[102];

    if (strlen(item) + strlen(menu) > 100) {
	strcpy(errmsg, G_("menu + item is limited to 100 chars"));
	return 5;
    }
    strcpy(mitem, menu); strcat(mitem, "/"); strcat(mitem, item);
    for (i = 0; i < nitems; i++) {
	if (strcmp(mitem, umitems[i]->name) == 0) break;
    }
    if (i == nitems) {
	strcpy(errmsg, G_("menu or item does not exist"));
	return 3;
    }
    delobj(umitems[i]->m);
    strcpy(umitems[i]->name, "invalid");
    free(umitems[i]->action);
    show(RConsole);
    return 0;
}
