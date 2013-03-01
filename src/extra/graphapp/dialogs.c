/*
 * GraphApp - Cross-Platform Graphics Programming Library.
 *
 * File: dialogs.c -- standard file dialogs and a few others.
 * Platform: Windows  Version: 2.42  Date: 1998/07/07
 *
 * Version: 1.00  Changes: Original version by Lachlan Patrick.
 * Version: 1.50  Changes: Uses new rectangles, callbacks.
 * Version: 2.20  Changes: Handle null strings correctly now.
 * Version: 2.40  Changes: Now uses YES, NO, CANCEL symbols.
 * Version: 2.42  Changes: Askstring fixed by Laurent Piguet.
 */

/* Copyright (C) 1993-1998 Lachlan Patrick

   This file is part of GraphApp, a cross-platform C graphics library.

   GraphApp is free software; you can redistribute it and/or modify it
   under the terms of the GNU Library General Public License.
   GraphApp is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY.

   See the file COPYLIB.TXT for details.
*/

/* Copyright (C) 2004--2008	The R Foundation

   Additions for R, Chris Jackson
   Find and replace dialog boxes and dialog handlers */

/* Mingw-w64 defines this to be 0x0502 */
#ifndef _WIN32_WINNT
# define _WIN32_WINNT 0x0500
#endif

#include "win-nls.h"
#include "internal.h"
#include "ga.h"

#include <shlobj.h>

typedef struct {
    char default_str[MAX_PATH];
    char question[40];
} browserInfo;

#define STATUSTEXT 14146

static int CALLBACK
InitBrowseCallbackProc( HWND hwnd, UINT uMsg, LPARAM lp, LPARAM lpData )
{
    char szDir[MAX_PATH], status[MAX_PATH + 40];

    if (uMsg == BFFM_INITIALIZED) {
	SendMessage(hwnd, BFFM_SETSELECTION, 1,
		    (LPARAM)&((browserInfo*)lpData)->default_str);
    } else if (uMsg == BFFM_SELCHANGED) {
	if (SHGetPathFromIDList((LPITEMIDLIST) lp ,szDir)) {
	    snprintf(status, MAX_PATH+40, "%s\n %s",
		     ((browserInfo*)lpData)->question, szDir);
	    SetDlgItemText(hwnd, STATUSTEXT, status);
	    SendMessage(hwnd, BFFM_ENABLEOK, 0, TRUE);
	} else
	    SendMessage(hwnd, BFFM_ENABLEOK, 0, FALSE);
    }
    return(0);
}

#define BUFSIZE _MAX_PATH
static char strbuf[BUFSIZE];
static wchar_t wcsbuf[65536];

static const char *filter[] = {
    "All Files (*.*)",	"*.*",
    "Text Files (*.TXT)",	"*.txt",
    "HTML Files (*.HTM)",	"*.htm",
    "PNG Files (*.PNG)",	"*.png",
    "JPEG Files (*.JPG)",	"*.jpg",
    "BMP Files (*.BMP)",	"*.bmp",
    ""
};

static const wchar_t *wfilter[] = {
    L"All Files (*.*)",	L"*.*",
    L"Text Files (*.TXT)",	L"*.txt",
    L"HTML Files (*.HTM)",	L"*.htm",
    L"PNG Files (*.PNG)",	L"*.png",
    L"JPEG Files (*.JPG)",	L"*.jpg",
    L"BMP Files (*.BMP)",	L"*.bmp",
    L""
};

unsigned int TopmostDialogs = 0; /* May be MB_TOPMOST */

static const char *userfilter;
static const wchar_t *userfilterW;

void setuserfilter(const char *uf)
{
    userfilter=uf;
}

void setuserfilterW(const wchar_t *uf)
{
    userfilterW=uf;
}

static HWND hModelessDlg = NULL;

int myMessageBox(HWND h, const char *text, const char *caption, UINT type)
{
    if(localeCP != GetACP()) {
	wchar_t wc[1000], wcaption[100];
	mbstowcs(wcaption, caption, 100);
	mbstowcs(wc, text, 1000);
	return MessageBoxW(h, wc, wcaption, type);
    } else
	return MessageBoxA(h, text, caption, type);
}

/*
 *  Error reporting dialog.
 */
void apperror(const char *errstr)
{
    if (! errstr)
	errstr = "Unspecified error";
    myMessageBox(0, errstr, "Graphics Library Error",
		 MB_TASKMODAL | MB_ICONSTOP | MB_OK | TopmostDialogs);
    exitapp();
}

void askok(const char *info)
{
    if (! info)
	info = "";
    myMessageBox(0, info, "Information",
		 MB_TASKMODAL | MB_ICONINFORMATION | MB_OK | TopmostDialogs);
}

int askokcancel(const char *question)
{
    int result;

    if (! question)
	question = "";
    result = myMessageBox(0, question, G_("Question"),
			  MB_TASKMODAL | MB_ICONQUESTION | MB_OKCANCEL | TopmostDialogs);

    switch (result) {
    case IDOK: result = YES; break;
    case IDCANCEL:
    default: result = CANCEL; break;
    }
    return result;
}

int askyesno(const char *question)
{
    int result;

    if (! question)
	question = "";
    result = myMessageBox(0, question, G_("Question"),
			  MB_TASKMODAL | MB_ICONQUESTION | MB_YESNO | TopmostDialogs);

    switch (result) {
    case IDYES: result = YES; break;
    case IDNO:  result = NO; break;
    default: result = CANCEL; break;
    }
    return result;
}

int askyesnocancel(const char *question)
{
    int result;

    if (! question)
	question = "";
    result = myMessageBox(0, question, G_("Question"),
			  MB_TASKMODAL | MB_ICONQUESTION | MB_YESNOCANCEL | MB_SETFOREGROUND | TopmostDialogs);

    switch (result) {
    case IDYES: result = YES; break;
    case IDNO:  result = NO; break;
    case IDCANCEL:
    default: result = CANCEL; break;
    }
    return result;
}

/* This should always have a native encoded name, so don't need Unicode here */
static char cod[MAX_PATH]=""; /*current open directory*/

void askchangedir()
{
    char *s, msg[MAX_PATH + 40];

/* set cod to current directory */
    GetCurrentDirectory(MAX_PATH, cod);
    s = askcdstring(G_(" Change working directory to:"), cod);
    if (s && (SetCurrentDirectory(s) == FALSE)) {
	snprintf(msg, MAX_PATH + 40,
		 G_("Unable to set '%s' as working directory"), s);
	askok(msg);
    }
    /* in every case reset cod (to new directory if all went ok
       or to old since user may have edited it) */
    GetCurrentDirectory(MAX_PATH, cod);
}

char *askfilename(const char *title, const char *default_name)
{
    if (*askfilenames(title, default_name, 0, userfilter?userfilter:filter[0], 0,
		      strbuf, BUFSIZE, NULL)) return strbuf;
    else return NULL;
}

char *askfilenamewithdir(const char *title, const char *default_name, const char *dir)
{
    if (*askfilenames(title, default_name, 0, 
		      userfilter ? userfilter : filter[0], 0,
		      strbuf, BUFSIZE, dir)) return strbuf;
    else return NULL;
}

char *askfilenames(const char *title, const char *default_name, int multi,
		   const char *filters, int filterindex,
		   char *strbuf, int bufsize,
		   const char *dir)
{
    int i;
    OPENFILENAME ofn;
    char cwd[MAX_PATH] = "";
    HWND prev = GetFocus();

    if (!default_name) default_name = "";
    strcpy(strbuf, default_name);
    GetCurrentDirectory(MAX_PATH, cwd);
    if (!cod[0]) strcpy(cod, cwd);

    ofn.lStructSize     = sizeof(OPENFILENAME);
    ofn.hwndOwner       = current_window ? current_window->handle : 0;
    ofn.hInstance       = 0;
    ofn.lpstrFilter     = filters;
    ofn.lpstrCustomFilter = NULL;
    ofn.nMaxCustFilter  = 0;
    ofn.nFilterIndex    = filterindex;
    ofn.lpstrFile       = strbuf;
    ofn.nMaxFile        = bufsize;
    ofn.lpstrFileTitle  = NULL;
    ofn.nMaxFileTitle   = _MAX_FNAME + _MAX_EXT;
    ofn.lpstrInitialDir = dir ? dir : cod;
    ofn.lpstrTitle      = title;
    ofn.Flags           = OFN_CREATEPROMPT | OFN_HIDEREADONLY | OFN_EXPLORER;
    if (multi) ofn.Flags |= OFN_ALLOWMULTISELECT;
    ofn.nFileOffset     = 0;
    ofn.nFileExtension  = 0;
    ofn.lpstrDefExt     = "*";
    ofn.lCustData       = 0L;
    ofn.lpfnHook        = NULL;
    ofn.lpTemplateName  = NULL;

    if (GetOpenFileName(&ofn) == 0) {
	if(!dir) GetCurrentDirectory(MAX_PATH, cod);
	SetCurrentDirectory(cwd);
	strbuf[0] = 0;
	strbuf[1] = 0;
    } else {
	if(!dir) GetCurrentDirectory(MAX_PATH, cod);
	SetCurrentDirectory(cwd);
	for (i = 0; i <  10; i++) if (peekevent()) doevent();
    }
    SetFocus(prev);
    return strbuf;
}

wchar_t *askfilenameW(const char *title, const char *default_name)
{
    wchar_t wtitle[1000], wdef_name[MAX_PATH];

    mbstowcs(wtitle, title, 1000);
    if (!default_name) wcscpy(wdef_name, L"");
    else mbstowcs(wdef_name, default_name, MAX_PATH);
    if (*askfilenamesW(wtitle, wdef_name, 0, 
		       userfilterW ? userfilterW : wfilter[0], 0,
		       NULL)) return wcsbuf;
    else return NULL;
}

wchar_t *askfilenamesW(const wchar_t *title, const wchar_t *default_name,
		       int multi,
		       const wchar_t *filters, int filterindex,
		       const wchar_t *dir)
{
    int i;
    OPENFILENAMEW ofn;
    char cwd[MAX_PATH];
    wchar_t wcod[MAX_PATH];
    HWND prev = GetFocus();

    if (!default_name) default_name = L"";
    wcscpy(wcsbuf, default_name);
    GetCurrentDirectory(MAX_PATH, cwd);
    if (!strcmp(cod, "")) {
	if (!dir) GetCurrentDirectoryW(MAX_PATH, wcod); else wcscpy(wcod, dir);
    } else
	mbstowcs(wcod, cod, MAX_PATH);

    ofn.lStructSize     = sizeof(OPENFILENAME);
    ofn.hwndOwner       = current_window ? current_window->handle : 0;
    ofn.hInstance       = 0;
    ofn.lpstrFilter     = filters;
    ofn.lpstrCustomFilter = NULL;
    ofn.nMaxCustFilter  = 0;
    ofn.nFilterIndex    = filterindex;
    ofn.lpstrFile       = wcsbuf;
    ofn.nMaxFile        = 65520; /* precaution against overflow */
    ofn.lpstrFileTitle  = NULL;
    ofn.nMaxFileTitle   = _MAX_FNAME + _MAX_EXT;
    ofn.lpstrInitialDir = wcod;
    ofn.lpstrTitle      = title;
    ofn.Flags           = OFN_CREATEPROMPT | OFN_HIDEREADONLY | OFN_EXPLORER;
    if (multi) ofn.Flags |= OFN_ALLOWMULTISELECT;
    ofn.nFileOffset     = 0;
    ofn.nFileExtension  = 0;
    ofn.lpstrDefExt     = L"*";
    ofn.lCustData       = 0L;
    ofn.lpfnHook        = NULL;
    ofn.lpTemplateName  = NULL;

    if (GetOpenFileNameW(&ofn) == 0) {
	/* This could fail if the Unicode name is not a native name */
	DWORD res = GetCurrentDirectory(MAX_PATH, cod);
	if(res) strcpy(cod, cwd);
	SetCurrentDirectory(cwd);
	wcsbuf[0] = 0;
	wcsbuf[1] = 0;
    } else {
	DWORD res = GetCurrentDirectory(MAX_PATH, cod);
	if(res) strcpy(cod, cwd);
	SetCurrentDirectory(cwd);
	for (i = 0; i <  10; i++) if (peekevent()) doevent();
    }
    SetFocus(prev);
    return wcsbuf;
}

int countFilenames(const char *list)
{
    const char *temp;
    int count;
    count = 0;
    for (temp = list; *temp; temp += strlen(temp)+1) count++;
    return count;
}

char *askfilesave(const char *title, const char *default_name)
{
    return askfilesavewithdir(title, default_name, NULL);
}

wchar_t *askfilesaveW(const char *title, const char *default_name) 
{
    int i;
    OPENFILENAMEW ofn;
    wchar_t cwd[MAX_PATH], wdef_name[MAX_PATH], wtitle[1000];

    if (!default_name) wcscpy(wdef_name, L"");
    else mbstowcs(wdef_name, default_name, MAX_PATH);
    wcscpy(wcsbuf, wdef_name);
    mbstowcs(wtitle, title, 1000);

    ofn.lStructSize     = sizeof(OPENFILENAME);
    ofn.hwndOwner       = current_window ? current_window->handle : 0;
    ofn.hInstance       = 0;
    ofn.lpstrFilter     = userfilterW ? userfilterW : wfilter[0];
    ofn.lpstrCustomFilter = NULL;
    ofn.nMaxCustFilter  = 0;
    ofn.nFilterIndex    = 0;
    ofn.lpstrFile       = wcsbuf;
    ofn.nMaxFile        = BUFSIZE;
    ofn.lpstrFileTitle  = NULL;
    ofn.nMaxFileTitle   = _MAX_FNAME + _MAX_EXT;
    if (GetCurrentDirectoryW(MAX_PATH, cwd))
	ofn.lpstrInitialDir = cwd;
    else
	ofn.lpstrInitialDir = NULL;
    ofn.lpstrTitle      = wtitle;
    ofn.Flags           = OFN_OVERWRITEPROMPT |
	OFN_NOCHANGEDIR | OFN_HIDEREADONLY;
    ofn.nFileOffset     = 0;
    ofn.nFileExtension  = 0;
    ofn.lpstrDefExt     = NULL;
    ofn.lCustData       = 0L;
    ofn.lpfnHook        = NULL;
    ofn.lpTemplateName  = NULL;

    if (GetSaveFileNameW(&ofn) == 0)
	return NULL;
    else {
	for (i = 0; i < 10; i++) if (peekevent()) doevent();
	return wcsbuf;
    }
}

char *askfilesavewithdir(const char *title, const char *default_name,
			 const char *dir)
{
    int i;
    OPENFILENAME ofn;
    char cwd[MAX_PATH], *defext = NULL;

    if (!default_name) default_name = "";
    else if(default_name[0] == '|') {
	defext = (char *)default_name + 2;
	default_name = "";
    }
    strcpy(strbuf, default_name);

    ofn.lStructSize     = sizeof(OPENFILENAME);
    ofn.hwndOwner       = current_window ? current_window->handle : 0;
    ofn.hInstance       = 0;
    ofn.lpstrFilter     = userfilter?userfilter:filter[0];
    ofn.lpstrCustomFilter = NULL;
    ofn.nMaxCustFilter  = 0;
    ofn.nFilterIndex    = 0;
    ofn.lpstrFile       = strbuf;
    ofn.nMaxFile        = BUFSIZE;
    ofn.lpstrFileTitle  = NULL;
    ofn.nMaxFileTitle   = _MAX_FNAME + _MAX_EXT;
    if(dir && strlen(dir) > 0) {
	strcpy(cwd, dir);
	/* This should have been set to use backslashes in the caller */
	ofn.lpstrInitialDir = cwd;
    } else {
	if (GetCurrentDirectory(MAX_PATH, cwd))
	    ofn.lpstrInitialDir = cwd;
	else
	    ofn.lpstrInitialDir = NULL;
    }
    ofn.lpstrTitle      = title;
    ofn.Flags           = OFN_OVERWRITEPROMPT |
	OFN_NOCHANGEDIR | OFN_HIDEREADONLY;
    ofn.nFileOffset     = 0;
    ofn.nFileExtension  = 0;
    ofn.lpstrDefExt     = defext;
    ofn.lCustData       = 0L;
    ofn.lpfnHook        = NULL;
    ofn.lpTemplateName  = NULL;

    if (GetSaveFileName(&ofn) == 0)
	return NULL;
    else {
	for (i = 0; i < 10; i++) if (peekevent()) doevent();
	return strbuf;
    }
}

/*
 *  Input a string using a dialog box.
 */

/*
 *  Dialog information structure:
 */
#define NOT_CHOSEN_YET -2

typedef struct dialog_data_class {
    int	hit;
    char *	result;
    label	question;
    field	text, pass;
    button	yes, no, cancel;
} dialog_data;

#define data(w) ((dialog_data *) (getdata(w)))

/*
 *  Some strings to use:
 */
/*	static char * OKAY_STRING	= "OK";
	static char * CANCEL_STRING	= "Cancel";
	static char * BROWSE_STRING	= "Browse"; */

static const char * QUESTION_TITLE	= "Question";
static const char * PASSWORD_TITLE	= "Password Entry";

static void add_data(window w)
{
    dialog_data *d;

    d = create (dialog_data);
    if (! d)
	return;
    d->hit = NOT_CHOSEN_YET;

    setdata(w, d);
}

static char * get_dialog_string(window w)
{
    dialog_data *d = data(w);

    if (d->hit < YES) /* cancelled */
	return NULL;
    del_string(d->result);
    if (d->text)	/* question dialog */
	d->result = new_string(GA_gettext(d->text));

    return d->result;
}

static void hit_button(control c)
{
    window w = parentwindow(c);
    dialog_data *d = data(w);
    int value = getvalue(c);

    d->hit = value;
    hide(w);
}

static void hit_key(window w, int key)
{
    button btn;
    char *name = NULL;

    w = parentwindow(w);

    if (data(w) == NULL)
	return;

    if ((btn = data(w)->yes) != NULL) {
	name = getname(btn);
	if ((key == '\n') || (tolower(name[0]) == tolower(key)))
	{
	    flashcontrol(btn);
	    activatecontrol(btn);
	    return;
	}
    }

    if ((btn = data(w)->cancel) != NULL) {
	name = getname(btn);
	if ((key == ESC) || (tolower(name[0]) == tolower(key)))
	{
	    flashcontrol(btn);
	    activatecontrol(btn);
	    return;
	}
    }

    if ((btn = data(w)->no) != NULL) {
	name = getname(btn);
	if ((key == ESC) || (tolower(name[0]) == tolower(key)))
	{
	    flashcontrol(btn);
	    activatecontrol(btn);
	    return;
	}
    }

}

/*
 *  Handle the events from a message dialog, hide the window afterwards.
 */
static int handle_message_dialog(window w)
{
    window old;
    dialog_data *d = data(w);

    old = currentdrawing();
    d->hit = NOT_CHOSEN_YET;

    show(w);
    while (d->hit == NOT_CHOSEN_YET) {
	if (!peekevent()) WaitMessage();
	doevent();
    }
    hide(w);

    if (old) drawto(old);

    return d->hit;
}

static window init_askstr_dialog(const char *title, const char *question,
				 const char *default_str)
{
    window win;
    dialog_data *d;
    int tw, bw, h, middle;

    if (! question)
	question= "";
    if (! default_str)
	default_str = "";

    tw = strwidth(SystemFont, G_("Cancel")) * 8;
    h = getheight(SystemFont);

    if (tw < 150) tw = 150;

    win = newwindow(title, rect(0,0,tw+30,h*9+12),
		    Titlebar | Centered | Modal);
    setbackground(win, dialog_bg());
    add_data(win);
    d = data(win);
    d->question = newlabel(question, rect(10,h,tw+4,h*2+2),
			   AlignLeft);
    if (title == PASSWORD_TITLE)
	d->text = newpassword(default_str, rect(10,h*4,tw+4,h*3/2));
    else
	d->text = newfield(default_str, rect(10,h*4,tw+4,h*3/2));

    middle = (tw+30)/2;
    bw = strwidth(SystemFont, G_("Cancel")) * 3/2;

    d->yes = newbutton(G_("OK"),
		       rect(middle-bw-10, h*7, bw, h+10), hit_button);
    setvalue(d->yes, YES);

    d->cancel = newbutton(G_("Cancel"),
			  rect(middle+10, h*7, bw, h+10), hit_button);
    setvalue(d->cancel, CANCEL);

    setkeydown(win, hit_key);

    return win;
}

char *askstring(const char *question, const char *default_str)
{
    static window win = NULL;
    window prev = current_window;

    if (! win)
	win = init_askstr_dialog(QUESTION_TITLE, question, default_str);
    else {
	settext(data(win)->question, question);
	settext(data(win)->text, default_str);
    }
    if (TopmostDialogs & MB_TOPMOST)
	BringToTop(win, 1);
    handle_message_dialog(win);
    current_window = prev;

    return get_dialog_string(win);
}

char *askcdstring(const char *question, const char *default_str)
{
    LPMALLOC g_pMalloc;
    BROWSEINFO bi;
    LPITEMIDLIST pidlBrowse;
    browserInfo info;
    OSVERSIONINFOEX osvi;

    strncpy(info.question, question, 40);
    strncpy(info.default_str, default_str, MAX_PATH);

    /* Get the shell's allocator. */
    if (!SUCCEEDED(SHGetMalloc(&g_pMalloc))) return NULL;

    osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);
    GetVersionEx((OSVERSIONINFO *)&osvi);

    ZeroMemory(&bi, sizeof(bi));
    bi.hwndOwner = 0;
    if(osvi.dwMajorVersion >= 6) { /* future proof */
	/* CSIDL_DESKTOP gets mapped to the User's desktop in Vista
	   (a bug).  SHGetFolderLocation is Win2k or later */
	if (!SUCCEEDED(SHGetFolderLocation(NULL, CSIDL_DRIVES, NULL, 0,
					   (LPITEMIDLIST *) &bi.pidlRoot)))
	    return NULL;
    }  /* else it is 0, which is CSIDL_DESKTOP */
    bi.pszDisplayName = strbuf;
    bi.lpszTitle = question;
    bi.ulFlags = BIF_RETURNONLYFSDIRS | BIF_USENEWUI;
    bi.lpfn = (BFFCALLBACK) InitBrowseCallbackProc;
    bi.lParam = (LPARAM) &info;

    /* Browse for a folder and return its PIDL. */
    pidlBrowse = SHBrowseForFolder(&bi);
    if (pidlBrowse != NULL) {
	SHGetPathFromIDList(pidlBrowse, strbuf);
	g_pMalloc->lpVtbl->Free(g_pMalloc, pidlBrowse);
	if (strbuf[0])
	    return strbuf;
    }
    return NULL;
}

char *askpassword(const char *question, const char *default_str)
{
    static window win = NULL;
    window prev = current_window;

    if (! win)
	win = init_askstr_dialog(PASSWORD_TITLE, question, default_str);
    else {
	settext(data(win)->question, question);
	settext(data(win)->text, default_str);
    }
    if (TopmostDialogs & MB_TOPMOST)
	BringToTop(win, 1);
    handle_message_dialog(win);
    current_window = prev;

    return get_dialog_string(win);
}

char *askUserPass(const char *title)
{
    static window win = NULL;
    dialog_data *d;
    window prev = current_window;

    if (! win) {
	int tw, bw, h, middle;

	tw = strwidth(SystemFont, G_("Cancel")) * 8;
	h = getheight(SystemFont);
	if (tw < 150) tw = 150;
	win = newwindow(title, rect(0, 0, tw+30, h*9+12),
			Titlebar | Centered | Modal);
	setbackground(win, dialog_bg());
	add_data(win);
	d = data(win);
	d->question = newlabel(G_("User"), rect(10, h, tw+4, h*2+2), AlignLeft);
	bw = strwidth(SystemFont, G_("Password"));
	d->text = newfield("", rect(20+bw, h, tw-6-bw, h*3/2));
	newlabel(_("Password"), rect(10, h*4, tw+4, h*2+2), AlignLeft);
	d->pass = newpassword("", rect(20+bw, h*4, tw-6-bw, h*3/2));
	middle = (tw+30)/2;
	bw = strwidth(SystemFont, G_("Cancel")) * 3/2;

	d->yes = newbutton(G_("OK"),
			   rect(middle-bw-10, h*7, bw, h+10), hit_button);
	setvalue(d->yes, YES);

	d->cancel = newbutton(G_("Cancel"),
			      rect(middle+10, h*7, bw, h+10), hit_button);
	setvalue(d->cancel, CANCEL);

	setkeydown(win, hit_key);
    } else {
	d = data(win);
	settext(d->text, "");
	settext(d->pass, "");
    }
    if (TopmostDialogs & MB_TOPMOST)
	BringToTop(win, 1);
    handle_message_dialog(win);
    current_window = prev;
    {
	char *user, *pass;
	static char buf[1000];
	if (d->hit < YES) /* cancelled */ return "";
	if (d->text) user = new_string(GA_gettext(d->text));
	else return "";
	if (d->pass) pass = new_string(GA_gettext(d->pass));
	else return "";
	snprintf(buf, 1000, "%s:%s", user, pass);
	return buf;
    }
    return ""; /* -Wall */
}

int modeless_active()
{
    if (hModelessDlg)
	return 1;
    return 0;
}

PROTECTED
HWND get_modeless()
{
    return hModelessDlg;
}

void finddialog(textbox t)
{
    static FINDREPLACE fr;
    static char szFindWhat[80];

    fr.lStructSize = sizeof(fr);
    fr.hwndOwner = t->handle;
    fr.lpstrFindWhat = szFindWhat;
    fr.wFindWhatLen = 80;
    fr.Flags = FR_DOWN;
    fr.lCustData        = 0 ;
    fr.lpfnHook         = NULL ;
    fr.lpTemplateName   = NULL ;

    hModelessDlg = FindText(&fr);
}

void replacedialog(textbox t)
{
    static FINDREPLACE fr;
    static char szFindWhat[80];
    static char szReplaceWith[80];

    fr.lStructSize = sizeof(fr);
    fr.hwndOwner = t->handle;
    fr.lpstrFindWhat = szFindWhat;
    fr.lpstrReplaceWith = szReplaceWith;
    fr.wFindWhatLen = 80;
    fr.wReplaceWithLen = 80;
    fr.Flags = FR_DOWN;
    fr.lCustData        = 0 ;
    fr.lpfnHook         = NULL ;
    fr.lpTemplateName   = NULL ;

    hModelessDlg = ReplaceText(&fr);
}


#include <richedit.h>
/* Find and select a string in a rich edit control */

static int richeditfind(HWND hwnd, char *what, int matchcase,
			int wholeword, int down)
{
    long start, end;
    CHARRANGE sel;
    WPARAM w = 0;
    FINDTEXTEX ft;
    sendmessage (hwnd, EM_EXGETSEL, 0, &sel) ;
    start = sel.cpMin;
    end = sel.cpMax;
    ft.lpstrText = what;
    ft.chrgText.cpMin = start;
    ft.chrgText.cpMax = end;
    if (down) {
	w = w | FR_DOWN;
	ft.chrg.cpMin = end;
	ft.chrg.cpMax = -1;
    }
    else {
	ft.chrg.cpMin = start;
	ft.chrg.cpMax = 0;
    }
    if (matchcase) w = w | FR_MATCHCASE;
    if (wholeword) w = w | FR_WHOLEWORD;
    if (sendmessage(hwnd, EM_FINDTEXTEX, w, &ft) == -1)
	return 0;
    else {
	sendmessage (hwnd, EM_EXSETSEL, 0, &(ft.chrgText));
	sendmessage (hwnd, EM_SCROLLCARET, 0, 0) ;
    }
    return 1;
}

static int richeditreplace(HWND hwnd, char *what, char *replacewith,
			   int matchcase, int wholeword, int down)
{
    /* If current selection is the find string, replace it and find next */
    long start, end;
    CHARRANGE sel;
    char *buf;
    textbox t = find_by_handle(hwnd);
    if (t) {
	sendmessage (hwnd, EM_EXGETSEL, 0, &sel) ;
	start = sel.cpMin;
	end = sel.cpMax;
	if (start < end) {
	    buf = (char *) malloc(end - start + 1);
	    sendmessage(hwnd, EM_GETSELTEXT, 0, buf);
	    if (!strcmp(buf, what)) {
		checklimittext(t, strlen(replacewith) - strlen(what) + 2);
		sendmessage (hwnd, EM_REPLACESEL, 1, replacewith);
	    }
	    free(buf);
	}
	/* else just find next */
	if (richeditfind(hwnd, what, matchcase, wholeword, down))
	    return 1;
    }
    return 0;
}

PROTECTED
void handle_findreplace(HWND hwnd, LPFINDREPLACE pfr)
{
    CHARRANGE sel;
    int matchcase=0, wholeword=0, down=0;
    char buf[100];
    if (pfr->Flags & FR_MATCHCASE) matchcase = 1;
    if (pfr->Flags & FR_WHOLEWORD) wholeword = 1;
    if (pfr->Flags & FR_DOWN) down = 1;

    if (pfr->Flags & FR_FINDNEXT) {
	if (!richeditfind(hwnd, pfr->lpstrFindWhat, matchcase, wholeword, down)) {
	    snprintf(buf, 100, G_("\"%s\" not found"), pfr->lpstrFindWhat);
	    askok(buf);
	}
    }
    else if (pfr->Flags & FR_REPLACE) {
	if (!richeditreplace(hwnd, pfr->lpstrFindWhat, pfr->lpstrReplaceWith, matchcase, wholeword, down)) {
	    snprintf(buf, 100, G_("\"%s\" not found"), pfr->lpstrFindWhat);
	    askok(buf);
	}
    }
    else if (pfr->Flags & FR_REPLACEALL) {
	/* replace all in the whole buffer then return to original selection state */
	sendmessage (hwnd, EM_EXGETSEL, 0, &sel) ;
	sendmessage (hwnd, EM_SETSEL, 0, 0) ;
	while ( richeditreplace(hwnd, pfr->lpstrFindWhat, pfr->lpstrReplaceWith, matchcase, wholeword, down) ) ;
	sendmessage (hwnd, EM_EXSETSEL, 0, &sel) ;
    }

    else if (pfr->Flags & FR_DIALOGTERM)
	hModelessDlg = NULL;
}
