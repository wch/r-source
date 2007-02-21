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

/* Copyright (C) 2004--2006 	The R Foundation

   Additions for R, Chris Jackson
   Find and replace dialog boxes and dialog handlers */

#define ENABLE_NLS 1
#include "../win-nls.h"
#include "internal.h"
#include "ga.h"

#include <shlobj.h>

static int CALLBACK
InitBrowseCallbackProc( HWND hwnd, UINT uMsg, LPARAM lParam, LPARAM lpData )
{
    if (uMsg == BFFM_INITIALIZED)
	SendMessage(hwnd, BFFM_SETSELECTION, 1, lpData);
    return(0);
}

/* browse for a folder under the Desktop, return the path in the argument */

static void selectfolder(char *folder, char *title)
{
    char buf[MAX_PATH];
    LPMALLOC g_pMalloc;
    HWND hwnd=0;
    BROWSEINFO bi;
    LPITEMIDLIST pidlBrowse;

    /* Get the shell's allocator. */
    if (!SUCCEEDED(SHGetMalloc(&g_pMalloc))) return;

    bi.hwndOwner = hwnd;
    bi.pidlRoot = NULL;
    bi.pszDisplayName = buf;
    bi.lpszTitle = title;
    bi.ulFlags = BIF_RETURNONLYFSDIRS;
    bi.lpfn = (BFFCALLBACK) InitBrowseCallbackProc;
    bi.lParam = (int) folder;

    /* Browse for a folder and return its PIDL. */
    pidlBrowse = SHBrowseForFolder(&bi);
    if (pidlBrowse != NULL) {
	SHGetPathFromIDList(pidlBrowse, folder);
        g_pMalloc->lpVtbl->Free(g_pMalloc, pidlBrowse);
    }
}


#define BUFSIZE _MAX_PATH
static char strbuf[BUFSIZE];

static char *filter[] = {
	"All Files (*.*)",	"*.*",
	"Text Files (*.TXT)",	"*.txt",
	"HTML Files (*.HTM)",	"*.htm",
	"PNG Files (*.PNG)",	"*.png",
	"JPEG Files (*.JPG)",	"*.jpg",
	"BMP Files (*.BMP)",	"*.bmp",
	""
};

unsigned int TopmostDialogs = 0; /* May be MB_TOPMOST */

static char *userfilter;
void setuserfilter(char *uf) {
   userfilter=uf;
}

static HWND hModelessDlg = NULL;

int myMessageBox(HWND h, char *text, char *caption, UINT type)
{
    if(is_NT && (localeCP != GetACP())) {
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
void apperror(char *errstr)
{
	if (! errstr)
		errstr = "Unspecified error";
	myMessageBox(0, errstr, "Graphics Library Error",
		MB_TASKMODAL | MB_ICONSTOP | MB_OK | TopmostDialogs);
	exitapp();
}

void askok(char *info)
{
	if (! info)
		info = "";
	myMessageBox(0, info, "Information",
		MB_TASKMODAL | MB_ICONINFORMATION | MB_OK | TopmostDialogs);
}

int askokcancel(char *question)
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

int askyesno(char *question)
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

int askyesnocancel(char *question)
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

static char cod[MAX_PATH]=""; /*current open directory*/

void askchangedir()
{
    char *s, msg[MAX_PATH + 40];

/* if cod has never been used, set it to current directory */
    if (!cod[0]) GetCurrentDirectory(MAX_PATH, cod);
    s = askcdstring(G_(" Change working directory to:"), cod);
    if (s && (SetCurrentDirectory(s) == FALSE)) {
	snprintf(msg, MAX_PATH + 40,
		 G_("Unable to set '%s' as working directory"), s);
	askok(msg);
    }
    /* in every case reset cod (to new directory if all went ok
       or to old since user may have edited it */
    GetCurrentDirectory(MAX_PATH, cod);
}

char *askfilename(char *title, char *default_name)
{
	if (*askfilenames(title, default_name, 0, userfilter?userfilter:filter[0], 0,
				          strbuf, BUFSIZE, NULL)) return strbuf;
	else return NULL;
}

char *askfilenamewithdir(char *title, char *default_name, char *dir)
{
	if (*askfilenames(title, default_name, 0, userfilter?userfilter:filter[0], 0,
				          strbuf, BUFSIZE, dir)) return strbuf;
	else return NULL;
}

char *askfilenames(char *title, char *default_name, int multi,
		   char *filters, int filterindex,
		   char *strbuf, int bufsize,
		   char *dir)
{
	int i;
	OPENFILENAME ofn;
        char cwd[MAX_PATH] = "";

	if (!default_name) default_name = "";
	strcpy(strbuf, default_name);
        GetCurrentDirectory(MAX_PATH, cwd);
        if (!strcmp(cod, "")) {
            if (!dir) strcpy(cod, cwd);
            else strcpy(cod, dir);
        }

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
        ofn.lpstrInitialDir = cod;
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
		GetCurrentDirectory(MAX_PATH, cod);
		SetCurrentDirectory(cwd);
		strbuf[0] = 0;
		strbuf[1] = 0;
		return strbuf;
	} else {
		GetCurrentDirectory(MAX_PATH, cod);
		SetCurrentDirectory(cwd);
		for (i = 0; i <  10; i++) if (peekevent()) doevent();
		return strbuf;
	}
}

int countFilenames(char *list)
{
	char *temp;
	int count;
	count = 0;
	for (temp = list; *temp; temp += strlen(temp)+1) count++;
	return count;
}

char *askfilesave(char *title, char *default_name)
{
    return askfilesavewithdir(title, default_name, NULL);
}

char *askfilesavewithdir(char *title, char *default_name, char *dir)
{
	int i;
	OPENFILENAME ofn;
        char *p, cwd[MAX_PATH], *defext = NULL;

	if (!default_name) default_name = "";
	else if(default_name[0] == '|') {
	    defext = default_name + 2;
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
	    for(p = cwd; *p; p++) if(*p == '/') *p = '\\';
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
		int 	hit;
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

	static char * QUESTION_TITLE	= "Question";
	static char * PASSWORD_TITLE	= "Password Entry";
	static char * FINDDIR_TITLE	= "Choose directory";

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

static void browse_button(control c)
{
    window w = parentwindow(c);
    dialog_data *d = data(w);
    char strbuf[MAX_PATH];
    strcpy(strbuf, GA_gettext(d->text));
    selectfolder(strbuf, G_("Choose a folder"));
    if(strlen(strbuf)) settext(d->text, strbuf);
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
	while (d->hit == NOT_CHOSEN_YET)
		doevent();
	hide(w);

	if (old) drawto(old);

	return d->hit;
}

static window init_askstr_dialog(char *title, char *question,
				 char *default_str)
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
	if (title == FINDDIR_TITLE) {
	    bw = strwidth(SystemFont, G_("Browse")) * 3/2;
	    d->text = newfield(default_str, rect(10,h*4,tw+4-bw,h*3/2));
	    newbutton(G_("Browse"), rect(20+tw-bw, h*4-2, bw, h+10),
		      browse_button);
	}
	else if (title == PASSWORD_TITLE)
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

char *askstring(char *question, char *default_str)
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

char *askcdstring(char *question, char *default_str)
{
	static window win = NULL;
	window prev = current_window;

	if (! win)
		win = init_askstr_dialog(FINDDIR_TITLE, question, default_str);
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

char *askpassword(char *question, char *default_str)
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

char *askUserPass(char *title)
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

void finddialog(textbox t){
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

void replacedialog(textbox t){
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


/* Find and select a string in a rich edit control */

int richeditfind(HWND hwnd, char *what, int matchcase, int wholeword, int down)
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

int richeditreplace(HWND hwnd, char *what, char *replacewith, int matchcase, int wholeword, int down)
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

