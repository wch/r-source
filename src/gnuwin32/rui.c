/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1998--2005  Guido Masarotto and Brian Ripley
 *  Copyright (C) 2004--2015  The R Foundation
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "win-nls.h"

#include <Defn.h>

/* R user interface based on GraphApp */
#include "Defn.h"
#undef append /* defined by graphapp/internal.h */
#include <stdio.h>
#undef DEBUG /* needed for mingw-runtime 2.0 */
/* the user menu code looks at the internal structure */
#define GA_EXTERN
#include "graphapp/internal.h"
#include "graphapp/ga.h"
#include "graphapp/stdimg.h"

#include "console.h"
#include "rui.h"
#include "preferences.h"
#include <Rversion.h>
#include "getline/wc_history.h"  /* for wgl_load/savehistory */
#include <Startup.h>          /* for SA_DEFAULT */

#define TRACERUI(a)

extern Rboolean UserBreak;

console RConsole = NULL;
int   RguiMDI = RW_MDI | RW_TOOLBAR | RW_STATUSBAR;
int   MDIset = 0;
window RFrame = NULL; /* some compilers want initialized for export */
rect MDIsize;
extern int ConsoleAcceptCmd, R_is_running;
extern Rboolean DebugMenuitem;
Rboolean R_LoadRconsole = TRUE; /* used in commandLineArgs */

static menubar RMenuBar;
static popup RConsolePopup;
static menuitem msource, mdisplay, mload, msave, mloadhistory,
    msavehistory, mpaste, mpastecmds, mcopy, mcopypaste, mlazy, mcomplete,
    mfncomplete, mconfig, mls, mrm, msearch, mde, mtools, mstatus;
static int lmanintro, lmanref, lmandata, lmanlang, lmanext, lmanint, 
    lmanadmin, lmanSweave;
static menu m;
static char cmd[1024];
static HelpMenuItems hmenu;
static PkgMenuItems pmenu;

#include "editor.h"

/* menu callbacks */

/* We need to handle \ in paths which are to be passed to R code.
   Since these can include \\ for network drives, we cannot just use /,
   although we did prior to R 2.4.0.

   MBCS-aware since 2.4.0.
 */
static void double_backslashes(char *s, char *out)
{
    char *p = s;

    int i;
    if(mbcslocale) {
	mbstate_t mb_st; int used;
	mbs_init(&mb_st);
	while((used = Mbrtowc(NULL, p, MB_CUR_MAX, &mb_st))) {
	    if(*p == '\\') *out++ = '\\';
	    for(i = 0; i < used; i++) *out++ = *p++;
	}
    } else
	for (; *p; p++)
	    if (*p == '\\') {*out++ = *p; *out++ = *p;} else *out++ = *p;
    *out = '\0';
}


void Rconsolecmd(char *cmd)
{
    consolecmd(RConsole, cmd);
}

static void closeconsole(control m)
{
    consolecmd(RConsole, "q()");
//    R_CleanUp(SA_DEFAULT, 0, 1);
}

static void quote_fn(wchar_t *fn, char *s)
{
    char *p = s;
    wchar_t *w;
    int used;
    for (w = fn; *w; w++) {
	if(*w  == L'\\') {
	    *p++ = '\\';
	    *p++ = '\\';
	} else {
	    used = wctomb(p, *w);
	    if(used > 0) p += used;
	    else {
		sprintf(p, "\\u%04x", (unsigned int) *w);
		p += 6;
	    }
	}
    }
    *p = '\0';
}


static void menusource(control m)
{
    wchar_t *fn;
    char local[MAX_PATH];

    if (!ConsoleAcceptCmd) return;
    setuserfilterW(L"R files (*.R)\0*.R\0S files (*.q, *.ssc, *.S)\0*.q;*.ssc;*.S\0All files (*.*)\0*.*\0\0");
    fn = askfilenameW(G_("Select file to source"), "");
    if (fn) {
	quote_fn(fn, local);
	snprintf(cmd, 1024, "source(\"%s\")", local);
	consolecmd(RConsole, cmd);
    } else show(RConsole);
}

static void menudisplay(control m)
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole,"local({fn<-choose.files(filters=Filters[c('R','txt','All'),],index=4)\nfile.show(fn,header=fn,title='')})");
}

static void menuloadimage(control m)
{
    wchar_t *fn;
    char s[MAX_PATH];

    if (!ConsoleAcceptCmd) return;
    setuserfilterW(L"R images (*.RData)\0*.RData\0R images - old extension (*.rda)\0*.rda\0All files (*.*)\0*.*\0\0");
    fn = askfilenameW(G_("Select image to load"), "");
    if (fn) {
	quote_fn(fn, s);
	snprintf(cmd, 1024, "load(\"%s\")", s);
	consolecmd(RConsole, cmd);
    } else show(RConsole);
}

static void menusaveimage(control m)
{
    wchar_t *fn;
    char s[MAX_PATH];

    if (!ConsoleAcceptCmd) return;
    setuserfilterW(L"R images (*.RData)\0*.RData\0All files (*.*)\0*.*\0\0");
    fn = askfilesaveW(G_("Save image in"), ".RData");
    if (fn) {
	quote_fn(fn, s);
	if (!strcmp(&s[strlen(s) - 2], ".*")) s[strlen(s) - 2] = '\0';
	snprintf(cmd, 1024, "save.image(\"%s\")", s);
	consolecmd(RConsole, cmd);
    } else show(RConsole);
}

static void menuloadhistory(control m)
{
    char *fn;

    setuserfilter("All files (*.*)\0*.*\0\0");
    fn = askfilename(G_("Load history from"), R_HistoryFile);
    if (fn) wgl_loadhistory(fn);
}

static void menusavehistory(control m)
{
    char *s;

    setuserfilter("All files (*.*)\0*.*\0\0");
    s = askfilesave(G_("Save history in"), R_HistoryFile);
    if (s) {
	R_setupHistory(); /* re-read the history size */
	wgl_savehistory(s, R_HistorySize);
    }
}

static void menuchangedir(control m)
{
    askchangedir();
}

static void menuprint(control m)
{
    consoleprint(RConsole);
}

static void menusavefile(control m)
{
    consolesavefile(RConsole, 0);
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

static void menutools(control m)
{
    if(ischecked(mtools)) {
	toolbar_hide();
	uncheck(mtools);
    } else {
	toolbar_show();
	check(mtools);
    }
}

void showstatusbar()
{
    if(ismdi() && !ischecked(mstatus)) {
	addstatusbar();
	check(mstatus);
    }
}


static void menustatus(control m)
{
    if(ischecked(mstatus)) {
	delstatusbar();
	uncheck(mstatus);
    } else {
	addstatusbar();
	check(mstatus);
    }
}


static void menulazy(control m)
{
    consoletogglelazy(RConsole);
/*    show(RConsole); */
}

extern void set_completion_available(int x);

static int filename_completion_on = 1;

static int check_file_completion(void)
{
    /* ought really to ask utils */
    return filename_completion_on;
}

static void menucomplete(control m)
{
    if(ischecked(mcomplete)) {
	set_completion_available(0);
	uncheck(mcomplete);
	uncheck(mfncomplete);
    } else {
	set_completion_available(-1);
	check(mcomplete);
	if(check_file_completion()) check(mfncomplete);
	else uncheck(mfncomplete);
    }
}

static void menufncomplete(control m)
{
    char cmd[200], *c0;
    if(ischecked(mfncomplete)) {
	c0 = "FALSE";
	uncheck(mfncomplete);
	filename_completion_on = 0;
    } else {
	c0 = "TRUE";
	check(mfncomplete);
	filename_completion_on = 1;
    }
    snprintf(cmd, 200, "utils::rc.settings(files=%s)", c0);
    consolecmd(RConsole, cmd);

}

static void menuconsolestayontop(control m)
{
    BringToTop(RConsole, 2);
}

static void menukill(control m)
{
    UserBreak = TRUE;
}

static void menukillall(control m)
{
    consolenewline(RConsole);
    Rf_jump_to_toplevel();
}

static Rboolean isdebuggerpresent(void)
{
    typedef BOOL (*R_CheckDebugger)(void);
    R_CheckDebugger entry;
    /* XP or later */
    entry =
	(R_CheckDebugger) GetProcAddress((HMODULE)GetModuleHandle("KERNEL32"),
					 "IsDebuggerPresent");
    if (entry == NULL) return(FALSE);
    else return (Rboolean) entry();
}

void breaktodebugger(void)
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
    else show(RConsole); 
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
	       "local({pkg <- select.list(sort(.packages(all.available = TRUE)),graphics=TRUE)\nif(nchar(pkg)) library(pkg, character.only=TRUE)})");
/*    show(RConsole); */
}

static void menupkgupdate(control m)
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole, "update.packages(ask='graphics',checkBuilt=TRUE)");
/*    show(RConsole); */
}

static void menupkgcranmirror(control m)
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole, "chooseCRANmirror()");
}

static void menupkgrepos(control m)
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole, "setRepositories()");
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
 
 
static void menucascade(control m)
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole, "utils::arrangeWindows(action='cascade')");
}

static void menutilehoriz(control m)
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole, "utils::arrangeWindows(action='horizontal')");
}

static void menutilevert(control m)
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole, "utils::arrangeWindows(action='vertical')");
}

static void menuminimizegroup(control m)
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole, "utils::arrangeWindows(action='minimize')");
}

static void menurestoregroup(control m)
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole, "utils::arrangeWindows(action='restore')");
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
    } else show(RConsole);
}

static void menumainman(control m)
{
    internal_shellexec("doc\\manual\\R-intro.pdf");
}

static void menumainref(control m)
{
    internal_shellexec("doc\\manual\\fullrefman.pdf");
}

static void menumaindata(control m)
{
    internal_shellexec("doc\\manual\\R-data.pdf");
}

static void menumainext(control m)
{
    internal_shellexec("doc\\manual\\R-exts.pdf");
}

static void menumainint(control m)
{
    internal_shellexec("doc\\manual\\R-ints.pdf");
}

static void menumainlang(control m)
{
    internal_shellexec("doc\\manual\\R-lang.pdf");
}

static void menumainadmin(control m)
{
    internal_shellexec("doc\\manual\\R-admin.pdf");
}

static void menumainSweave(control m)
{
    internal_shellexec("library\\utils\\doc\\Sweave.pdf");
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
    } else show(RConsole);
}

static void menusearchRsite(control m)
{
    char *s;
    static char olds[256] = "";

    if (!ConsoleAcceptCmd) return;
    s = askstring(G_("Search for words in help list archives and documentation"), olds);
    if (s && strlen(s)) {
	snprintf(cmd, 1024, "RSiteSearch(\"%s\")", s);
	if (strlen(s) > 255) s[255] = '\0';
	strcpy(olds, s);
	consolecmd(RConsole, cmd);
    } else show(RConsole);
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
    } else show(RConsole);
}

static void menuhelpstart(control m)
{
    if (!ConsoleAcceptCmd) return;
    consolecmd(RConsole, "help.start()");
/*    show(RConsole); 
    internal_shellexec("doc\\html\\index.html"); */
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
    char  s[256], s2[256];


    PrintVersionString(s2, 256);
    snprintf(s, 256, "%s\n%s %s %s",
	     s2,
	     "Copyright (C)", R_YEAR,
	     "The R Foundation for Statistical Computing");
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
void helpmenuact(HelpMenuItems hmenu)
{
    if (ConsoleAcceptCmd) {
	enable(hmenu->mhelp);
	enable(hmenu->mhelpstart);
	enable(hmenu->mhelpsearch);
	enable(hmenu->msearchRsite);
	enable(hmenu->mapropos);
	enable(hmenu->mCRAN);
    } else {
	disable(hmenu->mhelp);
	disable(hmenu->mhelpstart);
	disable(hmenu->mhelpsearch);
	disable(hmenu->msearchRsite);
	disable(hmenu->mapropos);
	disable(hmenu->mCRAN);
    }
}

void pkgmenuact(PkgMenuItems pmenu)
{
    if (ConsoleAcceptCmd) {
	enable(pmenu->mpkgl);
	enable(pmenu->mpkgm);
	enable(pmenu->mpkgi);
	enable(pmenu->mpkgil);
	enable(pmenu->mpkgu);
	enable(pmenu->mrepos);
    } else {
	disable(pmenu->mpkgl);
	disable(pmenu->mpkgm);
	disable(pmenu->mpkgi);
	disable(pmenu->mpkgil);
	disable(pmenu->mpkgu);
	disable(pmenu->mrepos);
    }
}

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

    } else {
	disable(msource);
	disable(mload);
	disable(msave);
	disable(mls);
	disable(mrm);
	disable(msearch);
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

    helpmenuact(hmenu);
    pkgmenuact(pmenu);

    draw(RMenuBar);
}

#define MCHECK(m) {if(!(m)) {del(RConsole); return 0;}}

void readconsolecfg()
{
    char  fn[128];
    int   sty = Plain;
    char  optf[PATH_MAX+1];

    struct structGUI gui;

    getDefaults(&gui);

    if (R_LoadRconsole) {
	snprintf(optf, PATH_MAX+1, "%s/Rconsole", getenv("R_USER"));
	if (!loadRconsole(&gui, optf)) {
	    snprintf(optf, PATH_MAX+1, "%s/etc/Rconsole", getenv("R_HOME"));
	    if (!loadRconsole(&gui, optf)) {
		app_cleanup();
		RConsole = NULL;
		exit(10);
	    }
	}
    }
    if (gui.tt_font) {
	strcpy(fn, "TT ");
	strcpy(fn+3, gui.font);
    } else strcpy(fn, gui.font);

    MDIsize = gui.MDIsize;

    if (gui.MDI)  RguiMDI |= RW_MDI;
    else          RguiMDI &= ~RW_MDI;

    if (MDIset == 1)  RguiMDI |= RW_MDI;
    if (MDIset == -1) RguiMDI &= ~RW_MDI;

    if (gui.toolbar) RguiMDI |= RW_TOOLBAR;
    else	     RguiMDI &= ~RW_TOOLBAR;
    if (gui.statusbar) RguiMDI |= RW_STATUSBAR;
    else	       RguiMDI &= ~RW_STATUSBAR;

    if (!strcmp(gui.style, "normal")) sty = Plain;
    if (!strcmp(gui.style, "bold")) sty = Bold;
    if (!strcmp(gui.style, "italic")) sty = Italic;

    Rwin_graphicsx = gui.grx;
    Rwin_graphicsy = gui.gry;

    if(strlen(gui.language)) {
	char *buf = malloc(50);
	snprintf(buf, 50, "LANGUAGE=%s", gui.language);
	putenv(buf);
    }
    setconsoleoptions(fn, sty, gui.pointsize, gui.crows, gui.ccols,
		      gui.cx, gui.cy,
		      gui.guiColors,
		      gui.prows, gui.pcols, gui.pagerMultiple, gui.setWidthOnResize,
		      gui.cbb, gui.cbl, gui.buffered, gui.cursor_blink);
}

static void dropconsole(control m, char *fn)
{
    char *p, local[MAX_PATH];

    p = Rf_strrchr(fn, '.');
    if(p) {
	/* OK even in MBCS */
	if(stricmp(p+1, "R") == 0) {
	    if(ConsoleAcceptCmd) {
		double_backslashes(fn, local);
		snprintf(cmd, 1024, "source(\"%s\")", local);
		consolecmd(RConsole, cmd);
	    }
	/* OK even in MBCS */
	} else if(stricmp(p+1, "RData") == 0 || stricmp(p+1, "rda")) {
	    if(ConsoleAcceptCmd) {
		double_backslashes(fn, local);
		snprintf(cmd, 1024, "load(\"%s\")", local);
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

int RguiPackageMenu(PkgMenuItems pmenu)
{
    MCHECK(newmenu(G_("Packages")));
    MCHECK(pmenu->mpkgl = newmenuitem(G_("Load package..."), 0, menupkgload));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(pmenu->mpkgm = newmenuitem(G_("Set CRAN mirror..."), 0,
			       menupkgcranmirror));
    MCHECK(pmenu->mrepos = newmenuitem(G_("Select repositories..."), 0,
				menupkgrepos));
    MCHECK(pmenu->mpkgi = newmenuitem(G_("Install package(s)..."), 0,
			       menupkginstallpkgs));
    MCHECK(pmenu->mpkgu = newmenuitem(G_("Update packages..."), 0,
			       menupkgupdate));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(pmenu->mpkgil = newmenuitem(G_("Install package(s) from local files..."),
				0, menupkginstalllocal));
    return 0;
}

static void CheckForManuals(void)
{
    lmanintro = check_doc_file("doc\\manual\\R-intro.pdf");
    lmanref = check_doc_file("doc\\manual\\fullrefman.pdf");
    lmandata = check_doc_file("doc\\manual\\R-data.pdf");
    lmanlang = check_doc_file("doc\\manual\\R-lang.pdf");
    lmanext = check_doc_file("doc\\manual\\R-exts.pdf");
    lmanint = check_doc_file("doc\\manual\\R-ints.pdf");
    lmanadmin = check_doc_file("doc\\manual\\R-admin.pdf");
    lmanSweave = check_doc_file("library\\utils\\doc\\Sweave.pdf");
}

/* Help functions common to all R windows.
   These should be appended to each context-specific help menu */

int RguiCommonHelp(menu m, HelpMenuItems hmenu)
{
    addto(m);

    MCHECK(hmenu->mFAQ = newmenuitem(G_("FAQ on R"), 0, menuFAQ));
    if (!check_doc_file("doc\\manual\\R-FAQ.html")) disable(hmenu->mFAQ);
    MCHECK(hmenu->mrwFAQ = newmenuitem(G_("FAQ on R for &Windows"), 0, menurwFAQ));
    if (!check_doc_file("doc\\html\\rw-FAQ.html")) disable(hmenu->mrwFAQ);


    if (!lmanintro && !lmanref && !lmandata && !lmanlang && !lmanext
       && !lmanint && !lmanadmin && !lmanSweave) {
	MCHECK(hmenu->mman0 = newmenuitem(G_("Manuals (in PDF)"), 0, NULL));
	disable(hmenu->mman0);
    } else {
	MCHECK(hmenu->mman = newsubmenu(m, G_("Manuals (in PDF)")));
	MCHECK(hmenu->mmanintro = newmenuitem("An &Introduction to R", 0,
				       menumainman));
	if (!lmanintro) disable(hmenu->mmanintro);
	MCHECK(hmenu->mmanref = newmenuitem("R &Reference", 0,
				     menumainref));
	if (!lmanref) disable(hmenu->mmanref);
	MCHECK(hmenu->mmandata = newmenuitem("R Data Import/Export", 0,
				      menumaindata));
	if (!lmandata) disable(hmenu->mmandata);
	MCHECK(hmenu->mmanlang = newmenuitem("R Language Definition", 0,
				      menumainlang));
	if (!lmanlang) disable(hmenu->mmanlang);
	MCHECK(hmenu->mmanext = newmenuitem("Writing R Extensions", 0,
				     menumainext));
	if (!lmanext) disable(hmenu->mmanext);
	MCHECK(hmenu->mmanint = newmenuitem("R Internals", 0,
				     menumainint));
	if (!lmanint) disable(hmenu->mmanint);
	MCHECK(hmenu->mmanadmin = newmenuitem("R Installation and Administration", 0,
				       menumainadmin));
	if (!lmanadmin) disable(hmenu->mmanadmin);
	MCHECK(hmenu->mmanSweave = newmenuitem("Sweave User", 0,
				       menumainSweave));
	if (!lmanSweave) disable(hmenu->mmanSweave);
    }


    addto(m);
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(hmenu->mhelp = newmenuitem(G_("R functions (text)..."), 0,
				      menuhelp));
    MCHECK(hmenu->mhelpstart = newmenuitem(G_("Html help"), 0, menuhelpstart));
    if (!check_doc_file("doc\\html\\index.html")) disable(hmenu->mhelpstart);
    MCHECK(hmenu->mhelpsearch = newmenuitem(G_("Search help..."), 0,
					    menuhelpsearch));
    MCHECK(hmenu->msearchRsite = newmenuitem("search.r-project.org ...", 0,
					     menusearchRsite));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(hmenu->mapropos = newmenuitem(G_("Apropos..."), 0, menuapropos));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(newmenuitem(G_("R Project home page"), 0, menuRhome));
    MCHECK(hmenu->mCRAN = newmenuitem(G_("CRAN home page"), 0, menuCRAN));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(newmenuitem(G_("About"), 0, menuabout));
    return 0;
}

static int RguiWindowMenu()
{
    if (ismdi())
	newmdimenu();
    else {
	MCHECK(newmenu(G_("Windows")));
	MCHECK(newmenuitem(G_("Cascade"), 0, menucascade));
	MCHECK(newmenuitem(G_("Tile &Horizontally"), 0, menutilehoriz));
	MCHECK(newmenuitem(G_("Tile &Vertically"), 0, menutilevert));
	MCHECK(newmenuitem(G_("Minimize group"), 0, menuminimizegroup));
	MCHECK(newmenuitem(G_("Restore group"), 0, menurestoregroup));
    }
    return 0;
}

#include <locale.h>

int setupui(void)
{
    char *p, *ctype, Rlocale[1000] = ""; /* Windows' locales can be very long */

    initapp(0, 0);

    /* set locale before doing anything with menus */
    setlocale(LC_CTYPE, ""); /* necessary in case next fails to set
				a valid locale */
    if((p = getenv("LC_ALL"))) strcpy(Rlocale, p);
    if((p = getenv("LC_CTYPE"))) strcpy(Rlocale, p);
    if (strcmp(Rlocale, "C") == 0) strcpy(Rlocale, "en");
    setlocale(LC_CTYPE, Rlocale);
    mbcslocale = MB_CUR_MAX > 1;
    ctype = setlocale(LC_CTYPE, NULL);
    p = strrchr(ctype, '.');
    if(p && isdigit(p[1])) localeCP = atoi(p+1); else localeCP = 1252;

    readconsolecfg();
    int flags = StandardWindow | Document | Menubar;
    if(mbcslocale) flags |= UseUnicode;
    if (RguiMDI & RW_MDI) {
	TRACERUI("Rgui");
	RFrame = newwindow(
#ifdef _WIN64
	    "RGui (64-bit)",
#else
	    "RGui (32-bit)",
#endif
	    MDIsize,
	    StandardWindow | Menubar | Workspace);
	setclose(RFrame, closeconsole);
	show(RFrame);
	TRACERUI("Rgui done");
	TRACERUI("Console");
	if (!(RConsole = newconsole("R Console", flags ))) return 0;
	TRACERUI("Console done");
    } else {
	TRACERUI("Console");
#ifdef _WIN64
	if (!(RConsole = newconsole("R Console (64-bit)", flags ))) return 0;
#else
	if (!(RConsole = newconsole("R Console (32-bit)", flags ))) return 0;
#endif
	TRACERUI("Console done");
    }
    
    if (ismdi()) {
	  int btsize = 24;
	  rect r = rect(2, 2, btsize, btsize);
	  control tb, bt;

	  MCHECK(tb = newtoolbar(btsize + 4));
	  addto(tb);

	  MCHECK(bt = newtoolbutton(open_image, r, menueditoropen));
	  MCHECK(addtooltip(bt, G_("Open script")));
	  r.x += (btsize + 1) ;

	  MCHECK(bt = newtoolbutton(open1_image, r, menuloadimage));
	  MCHECK(addtooltip(bt, G_("Load workspace")));
	  r.x += (btsize + 1) ;

	  MCHECK(bt = newtoolbutton(save_image, r, menusaveimage));
	  MCHECK(addtooltip(bt, G_("Save workspace")));
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
	TRACERUI("status bar");
	addstatusbar();
	addto(RConsole);
	TRACERUI("status bar done");
    }
    if (ismdi()) {
	char s[256];
	PrintVersionString(s, 256);
	setstatus(s);
    }
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
    MCHECK(msave = newmenuitem(G_("Save Workspace..."), 'S', menusaveimage));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(mloadhistory = newmenuitem(G_("Load History..."), 0,
				      menuloadhistory));
    MCHECK(msavehistory = newmenuitem(G_("Save History..."), 0,
				      menusavehistory));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(newmenuitem(G_("Change dir..."), 0, menuchangedir));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(newmenuitem(G_("Print..."), 'P', menuprint));
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
    if (ismdi()) {
	MCHECK(newmenu(G_("View")));
	MCHECK(mtools = newmenuitem(G_("Toolbar"), 0, menutools));
	MCHECK(mstatus = newmenuitem(G_("Statusbar"), 0, menustatus));
	if(RguiMDI & RW_TOOLBAR) check(mtools);
	if(RguiMDI & RW_STATUSBAR) check(mstatus);
    }
    MCHECK(newmenu(G_("Misc")));
    MCHECK(newmenuitem(G_("Stop current computation           \tESC"), 0,
		       menukill));
    MCHECK(newmenuitem(G_("Stop all computations"), 0, menukillall));
    if (DebugMenuitem || isdebuggerpresent())
	MCHECK(newmenuitem(G_("Break to debugger"), 0, menudebug));
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(mlazy = newmenuitem(G_("Buffered output"), 'W', menulazy));
    MCHECK(mcomplete = newmenuitem(G_("Word completion"), 0, menucomplete));
    check(mcomplete);
    MCHECK(mfncomplete = newmenuitem(G_("Filename completion"), 0,
				     menufncomplete));
    if(check_file_completion())
	check(mfncomplete);
    else
	uncheck(mfncomplete);
    MCHECK(newmenuitem("-", 0, NULL));
    MCHECK(mls = newmenuitem(G_("List objects"), 0, menuls));
    MCHECK(mrm = newmenuitem(G_("Remove all objects"), 0, menurm));
    MCHECK(msearch = newmenuitem(G_("List search &path"), 0, menusearch));

    pmenu = (PkgMenuItems) malloc(sizeof(struct structPkgMenuItems));
    RguiPackageMenu(pmenu);
    RguiWindowMenu();
    MCHECK(m = newmenu(G_("Help")));
    MCHECK(newmenuitem(G_("Console"), 0, menuconsolehelp));
    MCHECK(newmenuitem("-", 0, NULL));
    CheckForManuals();
    hmenu = (HelpMenuItems) malloc(sizeof(struct structHelpMenuItems));
    RguiCommonHelp(m, hmenu);
    consolesetbrk(RConsole, menukill, ESC, 0);
    wgl_hist_init(R_HistorySize, 0);
    if (R_RestoreHistory) wgl_loadhistory(R_HistoryFile);
    if (ismdi() && !(RguiMDI & RW_TOOLBAR)) toolbar_hide();
    show(RConsole);
    return 1;
}

static RECT RframeRect; /* for use by pagercreate */
RECT *RgetMDIsize(void)
{
    GetClientRect(hwndClient, &RframeRect);
    return &RframeRect;
}

int RgetMDIwidth(void)
{
    return RgetMDIsize()->right;
}

int RgetMDIheight(void)
{
    return RgetMDIsize()->bottom;
}


static menu *usermenus;
static char **usermenunames;

static Uitem  *umitems;

static int nmenus=0, nitems=0, alloc_menus=-1, alloc_items=-1;

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

menuItems *wingetmenuitems(const char *mname, char *errmsg) {
    menuItems *items;
    char mitem[1002], *p, *q, *r;
    int i,j = 0;

    q = (char *)malloc(1000 * sizeof(char));
    r = (char *)malloc(1000 * sizeof(char));

    if (strlen(mname) > 1000) {
	strcpy(errmsg, G_("'mname' is limited to 1000 bytes"));
	return NULL;
    }

    items = (menuItems *)malloc(sizeof(menuItems));
    if(nitems > 0)
	items->mItems = (Uitem *)malloc(alloc_items * sizeof(Uitem));

    strcpy(mitem, mname); strcat(mitem, "/");

    for (i = 0; i < nitems; i++) {
	p = strstr(umitems[i]->name, mitem);

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
	snprintf(r, 1000, "%s%s", mitem, umitems[i]->m->text);
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

static menu getMenu(const char * name)
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

int winaddmenu(const char *name, char *errmsg)
{
    const char *submenu = name;
    char *p, start[501];
    menu parent;

    if (getMenu(name))
	return 0;	/* Don't add repeats */

    if (nmenus >= alloc_menus) {
	if(alloc_menus <= 0) {
	    alloc_menus = 10;
	    usermenus = (menu *) malloc(sizeof(menu) * alloc_menus);
	    usermenunames = (char **) malloc(sizeof(char *) * alloc_menus);
	} else {
	    alloc_menus += 10;
	    usermenus = (menu *) realloc(usermenus, sizeof(menu) * alloc_menus);
	    usermenunames = (char **) realloc(usermenunames,
					      sizeof(char *) * alloc_menus);
	}
    }
    if (strlen(name) > 500) {
	strcpy(errmsg, G_("'menu' is limited to 500 bytes"));
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
	usermenunames[nmenus] = strdup(name);
	nmenus++;
	show(RConsole);
	return 0;
    } else {
	strcpy(errmsg, G_("failed to allocate menu"));
	return 1;
    }
}

int winaddmenuitem(const char * item, const char * menu,
		   const char * action, char *errmsg)
{
    int i, im;
    menuitem m;
    char mitem[1002], *p;

    /* if (nitems > 499) {
	strcpy(errmsg, G_("too many menu items have been created"));
	return 2;
	} */
    if (strlen(item) + strlen(menu) > 1000) {
	strcpy(errmsg, G_("menu + item is limited to 1000 bytes"));
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
	    if(alloc_items <= nitems) {
		if(alloc_items <= 0) {
		    alloc_items = 100;
		    umitems = (Uitem *) malloc(sizeof(Uitem) * alloc_items);
		} else {
		    alloc_items += 100;
		    umitems = (Uitem *) realloc(umitems,
						sizeof(Uitem) * alloc_items);
		}
	    }
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

int windelmenu(const char * menu, char *errmsg)
{
    int i, j, count = 0, len = strlen(menu);

    j = 0;
    for (i = 0; i < nmenus; i++) {
	if (strcmp(menu, usermenunames[i]) == 0
	  || (strncmp(menu, usermenunames[i], len) == 0 &&
	      usermenunames[i][len] == '/')) {
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

    for (j = nitems - 1; j >= 0; j--) {
	if (strncmp(menu, umitems[j]->name, len) == 0 &&
	    umitems[j]->name[len] == '/')
	    windelmenuitem(umitems[j]->name + len + 1, menu, errmsg);
    }


    show(RConsole);
    return 0;
}

void windelmenus(const char * prefix)
{
    int i, len = strlen(prefix);

    for (i = nmenus-1; i >=0; i--) {
	if (strncmp(prefix, usermenunames[i], len) == 0)
	    windelmenu(usermenunames[i], G_("menu not found"));
    }
}

int windelmenuitem(const char * item, const char * menu, char *errmsg)
{
    int i;
    char mitem[1002];

    if (strlen(item) + strlen(menu) > 1000) {
	strcpy(errmsg, G_("menu + item is limited to 1000 bytes"));
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
