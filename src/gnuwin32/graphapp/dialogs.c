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

#include "internal.h"

static char strbuf[256];

static char *filter[] = {
	"All Files (*.*)",	"*.*",
	"Text Files (*.TXT)",	"*.txt",
	"HTML Files (*.HTM)",	"*.htm",
	"PNG Files (*.PNG)",	"*.png",
	"JPEG Files (*.JPG)",	"*.jpg",
	"BMP Files (*.BMP)",	"*.bmp",
	""
};

static char *userfilter;
void setuserfilter(char *uf) {
   userfilter=uf;
}

/*
 *  Error reporting dialog.
 */
void apperror(char *errstr)
{
	if (! errstr)
		errstr = "Unspecified error";
	MessageBox(0, errstr, "Graphics Library Error",
		MB_TASKMODAL | MB_ICONSTOP | MB_OK);
	exitapp();
}

void askok(char *info)
{
	if (! info)
		info = "";
	MessageBox(0, info, "Information",
		MB_TASKMODAL | MB_ICONINFORMATION | MB_OK);
}

int askokcancel(char *question)
{
	int result;

	if (! question)
		question = "";
	result = MessageBox(0, question, "Question",
		MB_TASKMODAL | MB_ICONQUESTION | MB_OKCANCEL);

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
	result = MessageBox(0, question, "Question",
		MB_TASKMODAL | MB_ICONQUESTION | MB_YESNO);

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
	result = MessageBox(0, question, "Question",
		MB_TASKMODAL | MB_ICONQUESTION | MB_YESNOCANCEL);

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
    s = askcdstring(" Change working directory to:", cod);
    if (s && (SetCurrentDirectory(s) == FALSE)) {
	sprintf(msg, "Unable to set '%s' as working directory", s);
	askok(msg);
    }
    /* in every case reset cod (to new directory if all went ok
       or to old since user may have edited it */
    GetCurrentDirectory(MAX_PATH, cod);
}


char *askfilename(char *title, char *default_name)
{
	int i;
	OPENFILENAME ofn;
        char cwd[MAX_PATH]="";
	if (! default_name)
		default_name = "";
	strcpy(strbuf, default_name);
        GetCurrentDirectory(MAX_PATH,cwd);
        if (!strcmp(cod,"")) strcpy(cod,cwd);

	ofn.lStructSize     = sizeof(OPENFILENAME);
	ofn.hwndOwner       = current_window ?
				current_window->handle : 0;
	ofn.hInstance       = 0;
        ofn.lpstrFilter     = userfilter?userfilter:filter[0];
	ofn.lpstrCustomFilter = NULL;
	ofn.nMaxCustFilter  = 0;
	ofn.nFilterIndex    = 0;
	ofn.lpstrFile       = strbuf;
	ofn.nMaxFile        = _MAX_PATH;
	ofn.lpstrFileTitle  = NULL;
	ofn.nMaxFileTitle   = _MAX_FNAME + _MAX_EXT;
        ofn.lpstrInitialDir = cod;
	ofn.lpstrTitle      = title;
	ofn.Flags           = OFN_CREATEPROMPT | OFN_HIDEREADONLY;
	ofn.nFileOffset     = 0;
	ofn.nFileExtension  = 0;
	ofn.lpstrDefExt     = "*";
	ofn.lCustData       = 0L;
	ofn.lpfnHook        = NULL;
	ofn.lpTemplateName  = NULL;

	if (GetOpenFileName(&ofn) == 0) {
		GetCurrentDirectory(MAX_PATH,cod);
		SetCurrentDirectory(cwd);
		return NULL;
	} else {
		GetCurrentDirectory(MAX_PATH,cod);
		SetCurrentDirectory(cwd);
		for (i=0; i<10; i++)
			if (peekevent()) doevent();
		return strbuf;
	}
}

char *askfilesave(char *title, char *default_name)
{
    return askfilesavewithdir(title, default_name, NULL);
}

char *askfilesavewithdir(char *title, char *default_name, char *dir)
{
	int i;
	OPENFILENAME ofn;
        char cwd[MAX_PATH];

	if (! default_name)
		default_name = "";
	strcpy(strbuf, default_name);

	ofn.lStructSize     = sizeof(OPENFILENAME);
	ofn.hwndOwner       = current_window ?
				current_window->handle : 0;
	ofn.hInstance       = 0;
        ofn.lpstrFilter     = userfilter?userfilter:filter[0];
	ofn.lpstrCustomFilter = NULL;
	ofn.nMaxCustFilter  = 0;
	ofn.nFilterIndex    = 0;
	ofn.lpstrFile       = strbuf;
	ofn.nMaxFile        = _MAX_PATH;
	ofn.lpstrFileTitle  = NULL;
	ofn.nMaxFileTitle   = _MAX_FNAME + _MAX_EXT;
	if(dir && strlen(dir) > 0)
	    ofn.lpstrInitialDir = dir;
	else {
	    if (GetCurrentDirectory(MAX_PATH,cwd))
		ofn.lpstrInitialDir = cwd;
	    else
		ofn.lpstrInitialDir = NULL;
	}
	ofn.lpstrTitle      = title;
	ofn.Flags           = OFN_OVERWRITEPROMPT |
                              OFN_NOCHANGEDIR | OFN_HIDEREADONLY;
	ofn.nFileOffset     = 0;
	ofn.nFileExtension  = 0;
	ofn.lpstrDefExt     = "*";
	ofn.lCustData       = 0L;
	ofn.lpfnHook        = NULL;
	ofn.lpTemplateName  = NULL;

	if (GetSaveFileName(&ofn) == 0)
		return NULL;
	else {
		for (i=0; i<10; i++)
			if (peekevent()) doevent();
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
		field	text;
		button	yes, no, cancel;
	} dialog_data;

	#define data(w) ((dialog_data *) (getdata(w)))

/*
 *  Some strings to use:
 */
	static char * OKAY_STRING	= "OK";
	static char * CANCEL_STRING	= "Cancel";
	static char * BROWSE_STRING	= "Browse";

	static char * QUESTION_TITLE	= "Question";
	static char * PASSWORD_TITLE	= "Password Entry";
	static char * FINDDIR_TITLE	= "Change directory";

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
		d->result = new_string(gettext(d->text));

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

#ifndef OLD
extern void selectfolder(char *); /* from ../shext.c */

static void browse_button(control c)
{
    window w = parentwindow(c);
    dialog_data *d = data(w);
    char strbuf[MAX_PATH];
    
    selectfolder(strbuf);
    if(strlen(strbuf)) settext(d->text, strbuf);
}
#else
static void browse_button(control c)
{
    window w = parentwindow(c);
    dialog_data *d = data(w);

    OPENFILENAME ofn;
    char strbuf[256]="anything", *p;

    ofn.lStructSize     = sizeof(OPENFILENAME);
    ofn.hwndOwner       = 0;
    ofn.hInstance       = 0;
    ofn.lpstrFilter     = "All files (*.*)\0*.*\0\0";
    ofn.lpstrCustomFilter = NULL;
    ofn.nMaxCustFilter  = 0;
    ofn.nFilterIndex    = 0;
    ofn.lpstrFile       = strbuf;
    ofn.nMaxFile        = _MAX_PATH;
    ofn.lpstrFileTitle  = NULL;
    ofn.nMaxFileTitle   = _MAX_FNAME + _MAX_EXT;
    ofn.lpstrInitialDir = gettext(d->text);
    ofn.lpstrTitle      = "Select working directory";
    ofn.Flags           = OFN_HIDEREADONLY;
    ofn.nFileOffset     = 0;
    ofn.nFileExtension  = 0;
    ofn.lpstrDefExt     = "";
    ofn.lCustData       = 0L;
    ofn.lpfnHook        = NULL;
    ofn.lpTemplateName  = NULL;

    if(GetSaveFileName(&ofn) && strlen(strbuf)) {
	 p = strrchr(strbuf,'\\'); if(p) *p ='\0';
	 settext(d->text, strbuf);
    }
}
#endif

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

	tw = strwidth(SystemFont, CANCEL_STRING) * 8;
	h = getheight(SystemFont);

	if (tw < 150) tw = 150;

	win = newwindow(title, rect(0,0,tw+30,h*9+12),
			Titlebar | Centered | Modal);
        setbackground(win,LightGray);
	add_data(win);
	d = data(win);
	d->question = newlabel(question, rect(10,h,tw+4,h*2+2),
			AlignLeft);
	if (title == FINDDIR_TITLE) {
	    bw = strwidth(SystemFont, BROWSE_STRING) * 3/2;
	    d->text = newfield(default_str, rect(10,h*4,tw+4-bw,h*3/2));
	    newbutton(BROWSE_STRING, rect(20+tw-bw, h*4-2, bw, h+10),
		      browse_button);
	}
	else if (title == PASSWORD_TITLE)
		d->text = newpassword(default_str, rect(10,h*4,tw+4,h*3/2));
	else
		d->text = newfield(default_str, rect(10,h*4,tw+4,h*3/2));

	middle = (tw+30)/2;
	bw = strwidth(SystemFont, CANCEL_STRING) * 3/2;

	d->yes = newbutton(OKAY_STRING,
			rect(middle-bw-10, h*7, bw, h+10), hit_button);
	setvalue(d->yes, YES);

	d->cancel = newbutton(CANCEL_STRING,
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
	handle_message_dialog(win);
	current_window = prev;
	return get_dialog_string(win);
}

