/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file rwinst.c
 *  Copyright (C) 1999  B. D. Ripley
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

#include "graphapp.h"
#include "ga.h"
#include "Rversion.h"
#include <windows.h>
#include <string.h>

extern int _mkdir(const char* dir);
extern int Load_Unzip_Dll();
extern int do_unzip(char *zipname, char *dest, int nfiles, char **files,
		    int nxfiles, char **xfiles, int over);

/* text control for unzip output */
char *unztext;
int nunztext = 0;
#define NTEXT 16000
#define UNZSCROLL 4000

HINSTANCE hUnzipDll;

/* graphapp objects */
window w;
button bBack, bNext, bFinish, bCancel, bSrc, bDest;
radiobutton sys, pkg;
checkbox basepkg, texthelp, htmlhelp, ltxhelp, chmhelp, winhelp, srcsp, 
    pdf, refpdf, overwrite;
listbox packages;
textbox unzout;
label lVer, lsrc, ldest, lwhat1, lwhat2, lwarn2, lwarn3, lwarn4, lwarn5,
    lres3, lresp2, lwhat3;
field fRver, fSrc, fDest;

#ifndef RVER
#define RVER ""
#endif

static char Rversion[20];

int FullInstall = 1, over;
char Rver[20]=RVER, src[MAX_PATH], dest[MAX_PATH];
char selpkg[80], *pkglist[100], *selpkglist[100];
int npkgs, nspkgs, ispkgs, rwb=1, rwh=1, rwch=1, rww=0, rwl=0, rwwh=0, 
    rwsp=0, rwd=0, rwd2=0;
int prwb=1, prww=1, prwl=1, prwch=1, prwwh=0;

/* SHELLsort -- corrected from R. Sedgewick `Algorithms in C' */

void ssort(char **x, int n)
{
    char *v;
    int i, j, h;

    for (h = 1; h <= n / 9; h = 3 * h + 1);
    for (; h > 0; h /= 3)
	for (i = h; i < n; i++) {
	    v = x[i];
	    j = i;
	    while (j >= h && strcmp(x[j - h], v) > 0)
		 { x[j] = x[j - h]; j -= h; }
	    x[j] = v;
	}
}

/*
void fixslash(char *s)
{
    char *p;

    for (p = s; *p; p++) if (*p == '\\') *p = '/';
}
*/

void dosslash(char *s)
{
    char *p;

    for (p = s; *p; p++) if (*p == '/') *p = '\\';
}

#include <sys/stat.h>

int direxists(char * dir)
{
    struct stat sb;
    int res;
    char *p;

    dosslash(dir);
    /* remove trailing \, but leave c:\ alone */
    if(strlen(dir) > 3 && *(p = dir + strlen(dir) - 1) == '\\') *p = '\0';
    res = stat(dir, &sb);
    if(res != 0) return 0;
      return (sb.st_mode & _S_IFMT) == _S_IFDIR;
    return res == 0;
}

int fexists(char * file)
{
    struct stat sb;
    char str[MAX_PATH];
/* check in the source directory */

    strcpy(str, src);
    strcat(str, "/");
    strcat(str, file);
    dosslash(str);
    return stat(str, &sb) == 0;
}

void page1(), page2(), page3(), pagepkg1(), pagepkg2(), pagepkg3();
void cleanpage1(), cleanpage2(), cleanpage3(), cleanpagepkg1(),
    cleanpagepkg2(), cleanpagepkg3();

void finish(button b)
{
    exitapp();
}

void next1(button b)
{
    char str[MAX_PATH];
    FullInstall = ischecked(sys);
    strcpy(Rver, gettext(fRver));
    if(FullInstall && strcmp(Rver, Rversion) > 0) {
	sprintf(str, 
		"This installer is for version %s\nIt may not work for %s", 
		Rversion, Rver);
	askok(str);
    }
    if(FullInstall && strcmp(Rver, "rw0990") < 0) {
	sprintf(str, 
		"This installer is for version rw0990 and later only");
	askok(str);
	settext(fRver, Rversion);
 	return;
   }
    strcpy(src, gettext(fSrc));
    dosslash(src);
    settext(fSrc, src);
    if(strlen(src) == 0) {
	askok("You must give a source location");
	return;
    }
    if(!direxists(src)) {
	strcpy(str, "Directory ");
	strcat(str, src);
	strcat(str, " does not exist");
	askok(str);
	return;
    }
    strcpy(dest, gettext(fDest));
    dosslash(dest);
    settext(fDest, dest);
    if(strlen(dest) == 0) {
	askok("You must give a destination");
	return;
    }
    if(!direxists(dest)) {
	strcpy(str, "Directory ");
	strcat(str, dest);
	strcat(str, " does not exist. Create it?");
	if(askokcancel(str) != YES) return;
	if(_mkdir(dest)) {
	    strcpy(str, "Unable to create directory ");
	    strcat(str, dest);
	    askok(str);
	    return;
	}
    }
    if(!FullInstall && strcmp(Rver, dest + strlen(dest) - 6) == 0 ) {
	strcat(dest, "\\library");
    }
    
    cleanpage1();
    if(FullInstall) page2(); else pagepkg1();
}

void cleanpage1()
{
    delobj(sys);
    delobj(pkg);
    delobj(fRver);
    delobj(fSrc);
    delobj(fDest);
    delobj(lVer);
    delobj(lsrc);
    delobj(ldest);
    delobj(lwhat1);
    delobj(bSrc);
    delobj(bDest);
}

void cleanpage2()
{
    delobj(basepkg);
    delobj(texthelp);
    delobj(htmlhelp);
    delobj(ltxhelp);
    delobj(chmhelp);
    delobj(winhelp);
    delobj(srcsp);
    delobj(pdf);
    delobj(refpdf);
    delobj(lwhat2);
    delobj(lwarn2);
    delobj(overwrite);
}

void cleanpage3()
{
    delobj(lwarn3);
    delobj(lwarn4);
    delobj(lres3);
    delobj(unzout);
}

void cleanpagepkg1()
{
    int i;

    delobj(packages);
    delobj(lwarn5);
    for(i = 0; i < npkgs; i++) free(pkglist[i]);
    npkgs = 0;
}

void cleanpagepkg2()
{
    delobj(basepkg);
    delobj(htmlhelp);
    delobj(ltxhelp);
    delobj(chmhelp);
    delobj(winhelp);
    delobj(lwhat3);
    delobj(overwrite);
}

void cleanpagepkg3()
{
    delobj(unzout);
    delobj(lresp2);
}

void cancel(button b)
{
    exitapp();
}

void back2(button b)
{
    cleanpage2();
    page1();
}

void next2(button b)
{
    char str[MAX_PATH];

    rwb = ischecked(basepkg);
    rwh = ischecked(texthelp);
    rww = ischecked(htmlhelp);
    rwl = ischecked(ltxhelp);
    rwch = ischecked(chmhelp);
    rwwh = ischecked(winhelp);
    rwsp = ischecked(srcsp);
    rwd = ischecked(pdf);
    rwd2 = ischecked(refpdf);
    if(!rwb) {
	strcpy(str, dest);
	strcat(str, "/");
	strcat(str, Rver);
	strcat(str, "/bin");

	if(!direxists(str)) {
	    delobj(lwarn2);
	    lwarn2 =
		newlabel("R is not yet installed: please install it first",
			 rect(10, 210, 390, 20), AlignLeft);
	    return;
	}
    }
    if(!rwb & !rwh & !rww & !rwl & !rwch & !rwwh & !rwsp & !rwd & !rwd2) 
	return;
    over = ischecked(overwrite);
    cleanpage2();
    page3();
}

void back3(button b)
{
    cleanpage3();
    page2();
}

void backpkg1(button b)
{
    cleanpagepkg1();
    page1();
}

void nextpkg1(button b)
{
    int i;
    char *p;

    if(nspkgs > 0)
	for(i = 0; i < nspkgs; i++) free(selpkglist[i]);

    ispkgs = nspkgs = 0;
    for(i = 0; i < npkgs; i++)
	if(isselected(packages, i)) {
	    p = pkglist[i];
	    selpkglist[nspkgs] =
		(char *)malloc((strlen(p)+1) * sizeof(char));
	    strcpy(selpkglist[nspkgs], p);
	    nspkgs++;
	}
    if(nspkgs > 0) {
	strcpy(selpkg, selpkglist[0]);
	cleanpagepkg1();
	pagepkg2();
    } else {
	disable(bNext);
    }
    
}

void backpkg2(button b)
{
    cleanpagepkg2();
    pagepkg1();
}

void nextpkg2(button b)
{
    prwb = ischecked(basepkg);
    prww = ischecked(htmlhelp);
    prwl = ischecked(ltxhelp);
    prwch = ischecked(chmhelp);
    prwwh = ischecked(winhelp);
    if(!prwb & !prww & !prwl & !prwch & !prwwh) return;
    over = ischecked(overwrite);
    cleanpagepkg2();
    pagepkg3();
}

void backpkg3(button b)
{
    cleanpagepkg3();
    pagepkg1();
}

void nextpkg3(button b)
{
    cleanpagepkg3();
    pagepkg3();
}

void key1(control c, int ch)
{
    if(ch == '\n') next1(NULL);
    if(ch == ESC)  cancel(NULL);
}

void key2(control c, int ch)
{
    if(ch == '\n') next2(NULL);
    if(ch == 'b')  back2(NULL);
    if(ch == ESC)  cancel(NULL);
}

void key3(control c, int ch)
{
    if(ch == '\n') finish(NULL);
    if(ch == 'b')  back2(NULL);
    if(ch == ESC)  cancel(NULL);
}

void keypkg1(control c, int ch)
{
    if(ch == '\n') nextpkg1(NULL);
    if(ch == 'b') backpkg1(NULL);
    if(ch == ESC)  cancel(NULL);
}

void keypkg2(control c, int ch)
{
    if(ch == '\n') nextpkg2(NULL);
    if(ch == 'b') backpkg2(NULL);
    if(ch == ESC)  cancel(NULL);
}

void keypkg3(control c, int ch)
{
    if(ch == '\n') {
	if(ispkgs < nspkgs) nextpkg3(NULL); else finish(NULL);
    }
    if(ch == 'b') backpkg3(NULL);
    if(ch == ESC)  cancel(NULL);
}

void cSys(button b)
{
    enable(fRver);
}

void cPkg(button b)
{
    disable(fRver);
}

char selfile[50];

void browsesrc(button b)
{
    OPENFILENAME ofn;
    char strbuf[256]="anything", *p;

    strcpy(selfile, "");
    strcpy(src, gettext(fSrc));
    dosslash(src);

    ofn.lStructSize     = sizeof(OPENFILENAME);
    ofn.hwndOwner       = 0;
    ofn.hInstance       = 0;
    ofn.lpstrFilter     = "Zip files (*.zip)\0*.zip\0\0";
    ofn.lpstrCustomFilter = NULL;
    ofn.nMaxCustFilter  = 0;
    ofn.nFilterIndex    = 0;
    ofn.lpstrFile       = strbuf;
    ofn.nMaxFile        = _MAX_PATH;
    ofn.lpstrFileTitle  = NULL;
    ofn.nMaxFileTitle   = _MAX_FNAME + _MAX_EXT;
    ofn.lpstrInitialDir = src;
    ofn.lpstrTitle      = "Source directory";
    ofn.Flags           = OFN_HIDEREADONLY | OFN_PATHMUSTEXIST;
    ofn.nFileOffset     = 0;
    ofn.nFileExtension  = 0;
    ofn.lpstrDefExt     = "*";
    ofn.lCustData       = 0L;
    ofn.lpfnHook        = NULL;
    ofn.lpTemplateName  = NULL;

    if (!GetOpenFileName(&ofn) == 0) {
	 p = strrchr(strbuf, '\\'); if(p) *p ='\0';
	 if(strcmp("anything.*", p+1)) strcpy(selfile, p+1);
	 strcpy(src, strbuf);
	 settext(fSrc, src);
    }
}

void browsedest(button b)
{
    OPENFILENAME ofn;
    char strbuf[256]="anything", *p;

    strcpy(dest, gettext(fDest));
    dosslash(dest);

    ofn.lStructSize     = sizeof(OPENFILENAME);
    ofn.hwndOwner       = 0;
    ofn.hInstance       = 0;
    ofn.lpstrFilter     = "All files (*)\0*\0\0";
    ofn.lpstrCustomFilter = NULL;
    ofn.nMaxCustFilter  = 0;
    ofn.nFilterIndex    = 0;
    ofn.lpstrFile       = strbuf;
    ofn.nMaxFile        = _MAX_PATH;
    ofn.lpstrFileTitle  = NULL;
    ofn.nMaxFileTitle   = _MAX_FNAME + _MAX_EXT;
    ofn.lpstrInitialDir = dest;
    ofn.lpstrTitle      = "Destination directory";
    ofn.Flags           = OFN_HIDEREADONLY | OFN_PATHMUSTEXIST;
    ofn.nFileOffset     = 0;
    ofn.nFileExtension  = 0;
    ofn.lpstrDefExt     = "";
    ofn.lCustData       = 0L;
    ofn.lpfnHook        = NULL;
    ofn.lpTemplateName  = NULL;

    if(GetSaveFileName(&ofn) && strlen(strbuf)) {
	 p = strrchr(strbuf,'\\'); if(p) *p ='\0';
	 strcpy(dest, strbuf);
	 settext(fDest, dest);
    }
}

void header()
{
    label l;

    l = newlabel("Installer for R for Windows", rect(0, 10, 400, 30), Center);
    settextfont(l, newfont("Arial", SansSerif, -18));
}

void page1()
{
    int ypos;
    char tmp[5];

    clear(w); redraw(w); header();

/* NOTE: this depends on R_MINOR < 10 */
    if(!strlen(Rver)) {
	strcpy(tmp, R_MINOR); tmp[1] = '\0';
	sprintf(Rver, "rw%s0%s%s", R_MAJOR, tmp, tmp+2);
    }
    strcpy(Rversion, Rver);

    ypos = 50;
    lwhat1 = newlabel("What do you want to install?",
		      rect(10, ypos, 200, 20), AlignLeft);
    sys =   newradiobutton("R version", rect(100, ypos+30, 75, 20), cSys);
    fRver = newfield(Rver, rect(180, ypos+30, 75, 20));
    pkg =   newradiobutton("An add-on package",
			   rect(100, ypos+60, 200, 20), cPkg);
    if(FullInstall) 
	check(sys); 
    else {
	check(pkg); 
	disable(fRver);
    }

    ypos = 150;
    lsrc = newlabel("Source location:", rect(10, ypos+2, 80, 20), AlignRight);
    fSrc = newfield(src, rect(100, ypos, 200, 20));
    bSrc = newbutton("Browse", rect(310, ypos-2, 70, 24), browsesrc);

    ypos = 200;
    ldest = newlabel("Install directory:", rect(10, ypos+2, 80, 20),
		     AlignRight);
    fDest = newfield(dest, rect(100, ypos, 200, 20));
    bDest = newbutton("Browse", rect(310, ypos-2, 70, 24), browsedest);

    disable(bBack); hide(bBack);
    disable(bFinish); hide(bFinish);
    enable(bCancel); show(bCancel);
    enable(bNext); setaction(bNext, next1); show(bNext);
    setkeydown(w, key1);
    show(w);
}

void page2()
{
    int xpos = 80, ypos = 70, zips = 0;
    char str[MAX_PATH], str2[MAX_PATH];

    clear(w); redraw(w); header();

    lwhat2 = newlabel("Select the components you want to install",
		     rect(10, 50, 390, 20), AlignLeft);

    basepkg   = newcheckbox("base package", rect(xpos, ypos, 150, 20), NULL);
    strcpy(str, Rver); strcat(str, "b1.zip");
    strcpy(str2, Rver); strcat(str2, "b2.zip");
    if(!fexists(str) || !fexists(str2)) {
	uncheck(basepkg); disable(basepkg);
    } else {
	if(rwb) check(basepkg); else uncheck(basepkg);
	zips++;
    }

    ypos += 17;
    texthelp  = newcheckbox("plain text help",
			    rect(xpos, ypos, 150, 20), NULL);
    strcpy(str, Rver); strcat(str, "h.zip");
    if(!fexists(str)) {
	uncheck(texthelp); disable(texthelp);
    } else {
	if(rwh) check(texthelp); else uncheck(texthelp);
	zips++;
    }

    ypos += 17;
    chmhelp   = newcheckbox("Compiled HTML help", rect(xpos, ypos, 150, 20),
			    NULL);
    strcpy(str, Rver); strcat(str, "ch.zip");
    if(!fexists(str)) {
	uncheck(chmhelp); disable(chmhelp);
    } else {
	if(rwch) check(chmhelp); else uncheck(chmhelp);
	zips++;
    }

    ypos += 17;
    htmlhelp  = newcheckbox("HTML help", rect(xpos, ypos, 150, 20), NULL);
    strcpy(str, Rver); strcat(str, "w.zip");
    if(!fexists(str)) {
	uncheck(htmlhelp); disable(htmlhelp);
    } else {
	if(rww) check(htmlhelp); else uncheck(htmlhelp);
	zips++;
    }

    ypos += 17;
    ltxhelp = newcheckbox("latex help (for off-line printing)",
			    rect(xpos, ypos, 300, 20), NULL);
    strcpy(str, Rver); strcat(str, "l.zip");
    if(!fexists(str)) {
	uncheck(ltxhelp); disable(ltxhelp);
    } else {
	if(rwl) check(ltxhelp); else uncheck(ltxhelp);
	zips++;
    }

    ypos += 17;
    winhelp   = newcheckbox("Windows help", rect(xpos, ypos, 150, 20), NULL);
    strcpy(str, Rver); strcat(str, "wh.zip");
    if(!fexists(str)) {
	uncheck(winhelp); disable(winhelp); /* hide(winhelp);*/
    } else {
	if(rwwh) check(winhelp); else uncheck(winhelp);
	zips++;
    }

    ypos += 17;
    srcsp   = newcheckbox("files for building packages from source",
			  rect(xpos, ypos, 300, 20), NULL);
    strcpy(str, Rver); strcat(str, "sp.zip");
    if(!fexists(str)) {
	uncheck(srcsp); disable(srcsp); /* hide(srcsp);*/
    } else {
	if(rwsp) check(srcsp); else uncheck(srcsp);
	zips++;
    }

    ypos += 17;
    pdf   = newcheckbox("PDF manuals",
			  rect(xpos, ypos, 150, 20), NULL);
    strcpy(str, Rver); strcat(str, "d1.zip");
    if(!fexists(str)) {
	uncheck(pdf); disable(pdf); /* hide(pdf);*/
    } else {
	if(rwd) check(pdf); else uncheck(pdf);
	zips++;
    }
    refpdf   = newcheckbox("reference manual",
			   rect(xpos+150, ypos, 150, 20), NULL);
    strcpy(str, Rver); strcat(str, "d2.zip");
    if(!fexists(str)) {
	uncheck(refpdf); disable(refpdf); /* hide(pdf);*/
    } else {
	if(rwd2) check(refpdf); else uncheck(refpdf);
	zips++;
    }

    ypos += 25;
    lwarn2 = newlabel("Components that are not found will be greyed out",
		     rect(10, ypos, 390, 20), AlignLeft);

    enable(bBack); setaction(bBack, back2); show(bBack);
    hide(bFinish);
    enable(bCancel); show(bCancel);
    enable(bNext); setaction(bNext, next2); show(bNext);
    if(zips == 0) {
	disable(bNext);
	delobj(lwarn2);
	lwarn2 = newlabel("No components found in the source directory",
			  rect(10, ypos, 390, 20), AlignLeft);
	return;
    }
    if(!isenabled(basepkg)) {
	strcpy(str, dest);
	strcat(str, "/");
	strcat(str, Rver);
	strcat(str, "/bin");
	if(!direxists(str)) {
	    delobj(lwarn2);
	    lwarn2 =
		newlabel("R is not yet installed: please install it first",
			 rect(10, ypos, 390, 20), AlignLeft);
	disable(bNext);
	}
    }
    overwrite = newcheckbox("overwrite existing files?",
			    rect(30, 235, 150, 20), NULL);
    check(overwrite);
    setkeydown(w, key2);
    show(w);
}

void page3()
{
    char str[MAX_PATH], lab[100] = "", lab2[100], dest1[MAX_PATH];
    int rc;

    clear(w); redraw(w); header();

    lwarn3 = newlabel("Installing in:", rect(30, 50, 70, 20), AlignLeft);
    lwarn4 = newlabel(dest, rect(100, 50, 280, 20), AlignLeft);
    lres3 = newlabel(lab, rect(30, 240, 350, 20), AlignLeft);

    if(!nunztext) {
	unztext = (char *) malloc(NTEXT);
	nunztext = NTEXT;
    }
    strcpy(unztext, "");
    unzout = newtextarea(unztext, rect(20, 70, 350, 160));

    enable(bBack); setaction(bBack, back3); show(bBack);
    disable(bNext); hide(bNext);
    enable(bCancel); show(bCancel);
    enable(bFinish); show(bFinish);
    show(w);

    if(rwb) {
	strcpy(lab2, lab); delobj(lres3); strcat(lab, "base files . . . ");
	lres3 = newlabel(lab, rect(30, 240, 350, 20), AlignLeft);
	strcpy(dest1, dest);
	strcpy(str, src); strcat(str, "/");
	strcat(str, Rver); strcat(str, "b1.zip");
	rc = do_unzip(str, dest1, 0, NULL, 0, NULL, over);
	if(!rc) {
	    strcpy(dest1, dest);
	    strcpy(str, src); strcat(str, "/");
	    strcat(str, Rver); strcat(str, "b2.zip");
	    rc = do_unzip(str, dest1, 0, NULL, 0, NULL, over);	    
	}
	if(!rc) {
	    strcpy(lab, lab2); delobj(lres3); strcat(lab, "base files  ");
	    lres3 = newlabel(lab, rect(30, 240, 350, 20), AlignLeft);
	    rwb = 0;
	}
    }
    if(rwh) {
	strcpy(lab2, lab); delobj(lres3); strcat(lab, "text help . . .");
	lres3 = newlabel(lab, rect(30, 240, 350, 20), AlignLeft);
	strcpy(dest1, dest);
	strcpy(str, src); strcat(str, "/");
	strcat(str, Rver); strcat(str, "h.zip");
	rc = do_unzip(str, dest1, 0, NULL, 0, NULL, over);
	if(!rc) {
	    strcpy(lab, lab2); delobj(lres3); strcat(lab, "text help  ");
	    lres3 = newlabel(lab, rect(30, 240, 350, 20), AlignLeft);
	    rwh = 0;
	}
    }
    if(rww) {
	strcpy(lab2, lab); delobj(lres3); strcat(lab, "HTML help . . .");
	lres3 = newlabel(lab, rect(30, 240, 350, 20), AlignLeft);
	strcpy(dest1, dest);
	strcpy(str, src); strcat(str, "/");
	strcat(str, Rver); strcat(str, "w.zip");
	rc = do_unzip(str, dest1, 0, NULL, 0, NULL, over);
	if(!rc) {
	    strcpy(lab, lab2); delobj(lres3); strcat(lab, "HTML help  ");
	    lres3 = newlabel(lab, rect(30, 240, 350, 20), AlignLeft);
	    rwb = 0;
	}
    }
    if(rwl) {
	strcpy(lab2, lab); delobj(lres3); strcat(lab, "latex files . . .");
	lres3 = newlabel(lab, rect(30, 240, 350, 20), AlignLeft);
	strcpy(dest1, dest);
	strcpy(str, src); strcat(str, "/");
	strcat(str, Rver); strcat(str, "l.zip");
	rc = do_unzip(str, dest1, 0, NULL, 0, NULL, over);
	if(!rc) {
	    strcpy(lab, lab2); delobj(lres3); strcat(lab, "latex files  ");
	    lres3 = newlabel(lab, rect(30, 240, 350, 20), AlignLeft);
	    rwl = 0;
	}
    }
    if(rwch) {
	strcpy(lab2, lab); delobj(lres3); strcat(lab, "compiled HTML . . .");
	lres3 = newlabel(lab, rect(30, 240, 350, 20), AlignLeft);
	strcpy(dest1, dest);
	strcpy(str, src); strcat(str, "/");
	strcat(str, Rver); strcat(str, "ch.zip");
	rc = do_unzip(str, dest1, 0, NULL, 0, NULL, over);
	if(!rc) {
	    strcpy(lab, lab2); delobj(lres3); strcat(lab, "compiled HTML  ");
	    lres3 = newlabel(lab, rect(30, 240, 350, 20), AlignLeft);
	    rwch =0;
	}
    }
    if(rwwh) {
	strcpy(lab2, lab); delobj(lres3); strcat(lab, "winhelp . . .");
	lres3 = newlabel(lab, rect(30, 240, 350, 20), AlignLeft);
	strcpy(dest1, dest);
	strcpy(str, src); strcat(str, "/");
	strcat(str, Rver); strcat(str, "wh.zip");
	rc = do_unzip(str, dest1, 0, NULL, 0, NULL, over);
	if(!rc) {
	    strcpy(lab, lab2); delobj(lres3); strcat(lab, "winhelp  ");
	    lres3 = newlabel(lab, rect(30, 240, 350, 20), AlignLeft);
	    rwwh = 0;
	}
    }
    if(rwsp) {
	strcpy(lab2, lab); delobj(lres3); strcat(lab, "source . . .");
	lres3 = newlabel(lab, rect(30, 240, 350, 20), AlignLeft);
	strcpy(str, src); strcat(str, "/");
	strcat(str, Rver); strcat(str, "sp.zip");
	strcpy(dest1, dest);
	rc = do_unzip(str, dest1, 0, NULL, 0, NULL, over);
	if(!rc) {
	    strcpy(lab, lab2); delobj(lres3); strcat(lab, "source ");
	    lres3 = newlabel(lab, rect(30, 240, 350, 20), AlignLeft);
	    rwsp = 0;
	}
    }
    if(rwd) {
	strcpy(lab2, lab); delobj(lres3); strcat(lab, "PDF manuals . . .");
	lres3 = newlabel(lab, rect(30, 240, 350, 20), AlignLeft);
	strcpy(str, src); strcat(str, "/");
	strcat(str, Rver); strcat(str, "d1.zip");
	strcpy(dest1, dest);
	rc = do_unzip(str, dest1, 0, NULL, 0, NULL, over);
	if(!rc) {
	    strcpy(lab, lab2); delobj(lres3); strcat(lab, "docs ");
	    lres3 = newlabel(lab, rect(30, 240, 350, 20), AlignLeft);
	    rwd  = 0;
	}
    }
    if(rwd2) {
	strcpy(lab2, lab); delobj(lres3); strcat(lab, "ref manual . . .");
	lres3 = newlabel(lab, rect(30, 240, 350, 20), AlignLeft);
	strcpy(str, src); strcat(str, "/");
	strcat(str, Rver); strcat(str, "d2.zip");
	strcpy(dest1, dest);
	rc = do_unzip(str, dest1, 0, NULL, 0, NULL, over);
	if(!rc) {
	    strcpy(lab, lab2); delobj(lres3); strcat(lab, "refman ");
	    lres3 = newlabel(lab, rect(30, 240, 350, 20), AlignLeft);
	    rwd2 = 0;
	}
    }
    delobj(lres3);
    strcat(lab, "installed");
    lres3 = newlabel(lab, rect(30, 240, 350, 20), AlignLeft);
    setkeydown(w, key3);
    show(w);
}

void list1(listbox l, int item)
{
    int i, sum = 0;

    for(i = 0; i < npkgs; i++) if(isselected(packages, i)) sum++;
    if(sum > 0) enable(bNext); else disable(bNext);
}

#include <dirent.h>

void pagepkg1()
{
    DIR *dir;
    struct dirent *de;
    char *p, *p1;
    int boxht, i, ns;

    clear(w); redraw(w); header();

/* list zips in src */

    npkgs = 0;
    if ((dir = opendir(src)) == NULL) {
    } else {
	while ((de = readdir(dir))) {
	    p = de->d_name;
	    p1 = p + strlen(p) - 4;
	    if(!strcmp(p, "rw")) continue;
	    if(strcmp(p1, ".zip")) continue;
	    pkglist[npkgs] =
		(char *)malloc((strlen(p)+1) * sizeof(char));
	    strcpy(pkglist[npkgs], p);
	    *(pkglist[npkgs] + strlen(p) - 4) = '\0';
	    npkgs++;
	    if(npkgs >= 100) {
		askok("Only first 100 packages shown");
		break;
	    }
	}
    }
    if(npkgs) {
	lwarn5 = newlabel("zip files found:\nThese may be packages\n\nSelect one or more items:\nuse SHIFT or CTRL for more than one item",
			  rect(10, 50, 150, 80), Center);
	ssort(pkglist, npkgs);
	pkglist[npkgs] = (char *) NULL;
	boxht = min(200, npkgs*20);
	packages = newmultilist(pkglist, rect(200, 50, 150, boxht), list1);
	if((ns = strlen(selfile))) {
	    *(selfile+ns-4) = '\0';
	    for (i = 0; i < npkgs; i++)
		if(!strcmp(selfile, pkglist[i]))
		    setlistitem(packages, i);
	    enable(bNext);
	} else {
	    setlistitem(packages, -1);
	disable(bNext);
	}
	enable(bBack); setaction(bBack, backpkg1); show(bBack);
	disable(bFinish); hide(bFinish);
	enable(bCancel); show(bCancel);
	setaction(bNext, nextpkg1); show(bNext);
    } else {
	lwarn5 = newlabel("No zip files found", rect(100, 150, 200, 20),
			  Center);
	enable(bBack); setaction(bBack, backpkg1); show(bBack);
	disable(bFinish); hide(bFinish);
	enable(bCancel); show(bCancel);
	disable(bNext); show(bNext);
    }
    setkeydown(w, keypkg1);
    show(w);
}

void pagepkg2()
{
    int xpos = 80, ypos = 80;

    clear(w); redraw(w); header();

    lwhat3 = newlabel("Select the components you want to install",
		     rect(10, 50, 390, 20), AlignLeft);

    basepkg = newcheckbox("base", rect(xpos, ypos, 150, 20), NULL);
    if(prwb) check(basepkg); else uncheck(basepkg);

    ypos += 20;
    htmlhelp  = newcheckbox("HTML help", rect(xpos, ypos, 150, 20), NULL);
    if(prww) check(htmlhelp); else uncheck(htmlhelp);

    ypos += 20;
    ltxhelp = newcheckbox("latex help (for off-line printing)",
			    rect(xpos, ypos, 300, 20), NULL);
    if(prwl) check(ltxhelp); else uncheck(ltxhelp);

    ypos += 20;
    chmhelp   = newcheckbox("compiled HTML", rect(xpos, ypos, 150, 20), NULL);
    if(prwch) check(chmhelp); else uncheck(chmhelp);

    ypos += 20;
    winhelp   = newcheckbox("Windows help", rect(xpos, ypos, 150, 20), NULL);
    if(prwwh) check(winhelp); else uncheck(winhelp);

    overwrite = newcheckbox("overwrite existing files?",
			    rect(30, 220, 150, 20), NULL);
    uncheck(overwrite);

    enable(bBack); setaction(bBack, backpkg2); show(bBack);
    disable(bFinish); hide(bFinish);
    enable(bCancel); show(bCancel);
    setaction(bNext, nextpkg2); show(bNext);
    setkeydown(w, keypkg2);
    show(w);
}

void pagepkg3()
{
    int rc, nfiles, nxfiles;
    char zipname[MAX_PATH], cmd[MAX_PATH], *files[3], *xfiles[3], *p;

    clear(w); redraw(w); header();

    enable(bCancel); show(bCancel);
    disable(bBack); setaction(bBack, backpkg3); show(bBack);
    strcpy(selpkg, selpkglist[ispkgs++]);
    if(ispkgs < nspkgs) {
	hide(bFinish); disable(bNext); 
	setaction(bNext, nextpkg3); show(bNext);
    } else {
	hide(bNext); disable(bFinish); show(bFinish);
    }

    if(!nunztext) {
	unztext = (char *) malloc(NTEXT);
	nunztext = NTEXT;
    }
    strcpy(unztext, "installing "); strcat(unztext, selpkg);
    strcat(unztext, "\n\n");
    unzout = newtextarea(unztext, rect(20, 50, 350, 150));
    show(w);

    strcpy(zipname, src);
    strcat(zipname, "/"); strcat(zipname, selpkg); strcat(zipname, ".zip");
    nfiles = nxfiles = 0;
    if(prwb) {
	if(!prww) {
	    p = xfiles[nxfiles++] = (char*) malloc(50);
	    strcpy(p, selpkg); strcat(p, "/html/*");
	}
	if(!prwl) {
	    p = xfiles[nxfiles++] = (char*) malloc(50);
	    strcpy(p, selpkg); strcat(p, "/latex/*");
	}
	if(!prwch) {
	    p = xfiles[nxfiles++] = (char*) malloc(50);
	    strcpy(p, selpkg); strcat(p, "/chtml/*");
	}
	if(!prwwh) {
	    p = xfiles[nxfiles++] = (char*) malloc(50);
	    strcpy(p, selpkg); strcat(p, "/winhlp/*");
	}
    } else {
	if(prww) {
	    p = files[nfiles++] = (char*) malloc(50);
	    strcpy(p, selpkg); strcat(p, "/html/*");
	}
	if(prwl) {
	    p = files[nfiles++] = (char*) malloc(50);
	    strcpy(p, selpkg); strcat(p, "/latex/*");
	}
	if(prwch) {
	    p = files[nfiles++] = (char*) malloc(50);
	    strcpy(p, selpkg); strcat(p, "/chtml/*");
	}
	if(prwwh) {
	    p = files[nfiles++] = (char*) malloc(50);
	    strcpy(p, selpkg); strcat(p, "/winhlp/*");
	}
    }
    rc = do_unzip(zipname, dest, nfiles, files, nxfiles, xfiles, over);
    if(!rc) {
	strcpy(cmd, "Package ");
	strcat(cmd, selpkg);
	strcat(cmd, " installed");
	lresp2 = newlabel(cmd, rect(50, 220, 300, 20), Center);
    }
    enable(bBack); 
    if(ispkgs < nspkgs) 
	enable(bNext); 
    else {
	enable(bFinish);
	if(prww) askok("You will need to run\n\nlink.html.help()\n\nin R to update the\npackages list for help.start()");
    }
    setkeydown(w, keypkg3);
    show(w);
}

void init_interface(void)
{
    w = newwindow("R for Windows installer", rect(0, 0, 400, 300),
		  StandardWindow & ~Maximize);
    setbackground(w, LightGrey);
    bBack   = newbutton("< Back", rect(150, 260, 70, 25), back2);
    bNext   = newbutton("Next >", rect(230, 260, 70, 25), next1);
    bFinish = newbutton("Finish", rect(230, 260, 70, 25), finish);
    bCancel = newbutton("Cancel", rect(310, 260, 70, 25), cancel);
    page1();
}

/* This text buffer is limited: I guess to 16K from other examples. */

int WINAPI UnzDisplayBuf(LPSTR buf, unsigned long size)
{
    int total, firstvis;

    total = strlen(unztext) + 1 + size;
    if(total >= nunztext) {
	firstvis = UNZSCROLL;
	for (firstvis = UNZSCROLL; firstvis < total; firstvis++)
	    if(*(unztext + firstvis) == '\n') break;
	memmove(unztext, unztext + firstvis, strlen(unztext) + 1 - firstvis);
    }
    strncat(unztext, (char *) buf, size);
    settext(unzout, unztext);
    scrolltext(unzout, 8); /* show bottom 8 lines */
    show(unzout);
    return (unsigned int) size;
}

int main(void)
{
    GetCurrentDirectory(MAX_PATH, src);
    strcpy(dest, src);
    init_interface();
    gamainloop();
    return 0;
}
