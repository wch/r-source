/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1998-2002   Lyndon Drake
 *                            and the R Development Core Team
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
# include <config.h>
#endif

#include "Defn.h"
#include "Fileio.h"
#ifdef HAVE_FCNTL_H
# include <fcntl.h> /* for open and constants */
#endif
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

#include "terminal.h"
#include "terminal-prefs.h"
#include "gtkconsole.h"

#include <gnome.h>

typedef struct pager_data_ts pager_data_t;
struct pager_data_ts {
  GtkWidget *pagerwin;
  GtkWidget *text;
};

static void pagertb_print(GtkWidget *widget, gpointer data)
{
  /* FIXME */
}

static void pagertb_copy(GtkWidget *widget, gpointer data)
{
  pager_data_t *pager_data;

  if(data != NULL) {
    pager_data = (pager_data_t *) data;

    gtk_editable_copy_clipboard(GTK_EDITABLE(pager_data->text));
  }
}

static void pagertb_start(GtkWidget *widget, gpointer data)
{
  pager_data_t *pager_data;
  GdkEventKey event;
  gboolean retval;

  if(data != NULL) {
    pager_data = (pager_data_t *) data;

    event.keyval = GDK_Home;
    event.state |= GDK_CONTROL_MASK;

    gtk_signal_emit_by_name(GTK_OBJECT(pager_data->text), "key_press_event",
			    &event,
			    &retval);
  }
}

static void pagertb_pageup(GtkWidget *widget, gpointer data)
{
  pager_data_t *pager_data;
  GdkEventKey event;
  gboolean retval;

  if(data != NULL) {
    pager_data = (pager_data_t *) data;

    event.keyval = GDK_Page_Up;

    gtk_signal_emit_by_name(GTK_OBJECT(pager_data->text), "key_press_event",
			    &event,
			    &retval);
  }
}

static void pagertb_pagedown(GtkWidget *widget, gpointer data)
{
  pager_data_t *pager_data;
  GdkEventKey event;
  gboolean retval;

  if(data != NULL) {
    pager_data = (pager_data_t *) data;

    event.keyval = GDK_Page_Down;

    gtk_signal_emit_by_name(GTK_OBJECT(pager_data->text), "key_press_event",
			    &event,
			    &retval);
  }
}

static void pagertb_end(GtkWidget *widget, gpointer data)
{
  pager_data_t *pager_data;
  GdkEventKey event;
  gboolean retval;

  if(data != NULL) {
    pager_data = (pager_data_t *) data;

    event.keyval = GDK_End;
    event.state |= GDK_CONTROL_MASK;

    gtk_signal_emit_by_name(GTK_OBJECT(pager_data->text), "key_press_event",
			    &event,
			    &retval);
  }
}

static void pagertb_close(GtkWidget *widget, gpointer data)
{
  pager_data_t *pager_data;

  if(data != NULL) {
    pager_data = (pager_data_t *) data;
    gtk_widget_destroy(GTK_WIDGET(pager_data->pagerwin));
  }
}

static GnomeUIInfo pager_toolbar[] =
{
  GNOMEUIINFO_ITEM_STOCK("Print", "Print pager text", pagertb_print,
			 GNOME_STOCK_PIXMAP_PRINT),
  GNOMEUIINFO_SEPARATOR,
  GNOMEUIINFO_ITEM_STOCK("Copy", "Copy the selection", pagertb_copy,
			 GNOME_STOCK_PIXMAP_COPY),
  GNOMEUIINFO_SEPARATOR,
  GNOMEUIINFO_ITEM_STOCK("Top", "Scroll to the top", pagertb_start,
			 GNOME_STOCK_PIXMAP_TOP),
  GNOMEUIINFO_ITEM_STOCK("Page Up", "Scroll up one page", pagertb_pageup,
			 GNOME_STOCK_PIXMAP_UP),
  GNOMEUIINFO_ITEM_STOCK("Page Down", "Scroll down one page", 
			 pagertb_pagedown, GNOME_STOCK_PIXMAP_DOWN),
  GNOMEUIINFO_ITEM_STOCK("Bottom", "Scroll to the end", pagertb_end, 
			 GNOME_STOCK_PIXMAP_BOTTOM),
  GNOMEUIINFO_SEPARATOR,
  GNOMEUIINFO_ITEM_STOCK("Close", "Close pager", pagertb_close,
			 GNOME_STOCK_PIXMAP_CLOSE),
  GNOMEUIINFO_END
};



void pager_set_style()
{
  /*
  GtkStyle *textstyle;

  textstyle = gtk_style_copy(gtk_widget_get_style(pager_data->text));
  gdk_font_unref(textstyle->font);
  textstyle->font = gdk_font_load(prefs_get_pager_text_font());
  textstyle->text[GTK_STATE_NORMAL] = prefs_get_pager_text_textcolor();
  textstyle->base[GTK_STATE_NORMAL] = prefs_get_pager_text_bgcolor();
  gtk_widget_set_style(pager_data->text, textstyle);


  // load title and em font here
  titlefont = gdk_font_load(prefs_get_pager_title_font());
  emfont = gdk_font_load(prefs_get_pager_em_font());

  // set width to 80 columns here
  charw = gdk_char_width(pager_data->text->style->font, 'w');
  charh = gdk_char_height(pager_data->text->style->font, 'H');
  winw = 83 * charw;
  winh = 50 * charh;
  gtk_widget_set_usize(pager_data->text, winw, winh);

  gtk_text_set_editable (GTK_TEXT (pager_data->text), FALSE);
  gtk_table_attach (GTK_TABLE (table), pager_data->text, 0, 1, 0, 1,
		    GTK_EXPAND | GTK_SHRINK | GTK_FILL,
		    GTK_EXPAND | GTK_SHRINK | GTK_FILL, 0, 0);

  vscrollbar = gtk_vscrollbar_new (GTK_TEXT (pager_data->text)->vadj);
  gtk_table_attach (GTK_TABLE (table), vscrollbar, 1, 2, 0, 1,
		    GTK_FILL, 
		    GTK_EXPAND | GTK_SHRINK | GTK_FILL, 0, 0);

  textcolor = prefs_get_pager_title_textcolor();
  bgcolor = prefs_get_pager_title_bgcolor();

  for(i = 0; i < nfile; i++) {
    if((title[i] != NULL) && (*title[i] != '\0')) {
      g_snprintf(buf, bufsize, "%s\n\n", title[i]);
      gtk_text_insert(GTK_TEXT(pager_data->text), 
		      titlefont,
		      &textcolor, 
		      &bgcolor,
		      buf, strlen(buf));
    }
    if((fd = open(file[i], O_RDONLY, "")) != -1) {
      do {
	readlen = read(fd, buf, bufsize);

	emmode = FALSE;
	modestart = buf;

	// strip backspaced stuff 
	if(*buf == '\b')
	  *buf = ' ';
	for(j = buf, k = buf; j < buf + readlen; j++) {
	  if(*j == '\b') {
	    k--;
	    if(k != modestart)
	      gtk_text_insert(GTK_TEXT(pager_data->text), NULL, NULL, NULL,
			      modestart, k - modestart);
	    modestart = k;
	    emmode = TRUE;
	  }
	  else {
	    *k = *j;
	    k++;
	    if(emmode) {
	      gtk_text_insert(GTK_TEXT(pager_data->text),
			      emfont,
			      NULL, 
			      NULL,
			      k - 1, 1);
	      modestart = k;
	      emmode = FALSE;
	    }
	  }
	}

	gtk_text_insert(GTK_TEXT(pager_data->text), NULL, NULL, NULL,
			modestart, k - modestart);
      } while(readlen == bufsize);
    }
    else {
      g_snprintf(buf, bufsize, "NO FILE %s\n\n", file[i]);
      gtk_text_insert(GTK_TEXT(pager_data->text), NULL, NULL, NULL,
		      buf, strlen(buf));
    }
  }
  */
}

int Rgnome_ShowFiles(int nfile, char **file, char **title, char *wtitle,
		     Rboolean del, char *pager) 
{
  pager_data_t *pager_data;
  GtkWidget *table, *vscrollbar;
  gchar *realtitle;
  GtkStyle *textstyle;
  gint charw, charh, winw, winh;
  GdkFont *titlefont, *emfont;
  GdkColor textcolor, bgcolor;

  const gint bufsize = 2048;
  gchar buf[bufsize];
  gint i;
  gint fd, readlen;
  gchar *j, *k;
  gboolean emmode;
  gchar *modestart;

  if(nfile < 1)
    return 0;

  if((wtitle != NULL) && (*wtitle != '\0'))
    realtitle = wtitle;
  else
    realtitle = "R pager";

  pager_data = g_new(pager_data_t, 1);

  pager_data->pagerwin = gnome_app_new("R.pager", realtitle);
  
  gnome_app_create_toolbar_with_data(GNOME_APP(pager_data->pagerwin), pager_toolbar, (gpointer) pager_data);

  table = gtk_table_new(1, 2, FALSE);
  gtk_table_set_col_spacing (GTK_TABLE (table), 0, 2);

  pager_data->text = gtk_text_new(NULL, NULL);

  /* set text font here */
  textstyle = gtk_style_copy(gtk_widget_get_style(pager_data->text));
  textstyle->font = gdk_font_load(prefs_get_pager_text_font());
  textstyle->text[GTK_STATE_NORMAL] = prefs_get_pager_text_textcolor();
  textstyle->base[GTK_STATE_NORMAL] = prefs_get_pager_text_bgcolor();
  gtk_widget_set_style(pager_data->text, textstyle);

  /* load title and em font here */
  titlefont = gdk_font_load(prefs_get_pager_title_font());
  emfont = gdk_font_load(prefs_get_pager_em_font());

  /* set width to 80 columns here */
  charw = gdk_char_width(pager_data->text->style->font, 'w');
  charh = gdk_char_height(pager_data->text->style->font, 'H');
  winw = 83 * charw;
  winh = 50 * charh;
  gtk_widget_set_usize(pager_data->text, winw, winh);

  gtk_text_set_editable (GTK_TEXT (pager_data->text), FALSE);
  gtk_table_attach (GTK_TABLE (table), pager_data->text, 0, 1, 0, 1,
		    GTK_EXPAND | GTK_SHRINK | GTK_FILL,
		    GTK_EXPAND | GTK_SHRINK | GTK_FILL, 0, 0);

  vscrollbar = gtk_vscrollbar_new (GTK_TEXT (pager_data->text)->vadj);
  gtk_table_attach (GTK_TABLE (table), vscrollbar, 1, 2, 0, 1,
		    GTK_FILL, 
		    GTK_EXPAND | GTK_SHRINK | GTK_FILL, 0, 0);

  textcolor = prefs_get_pager_title_textcolor();
  bgcolor = prefs_get_pager_title_bgcolor();

  for(i = 0; i < nfile; i++) {
    if((title[i] != NULL) && (*title[i] != '\0')) {
      g_snprintf(buf, bufsize, "%s\n\n", title[i]);
      gtk_text_insert(GTK_TEXT(pager_data->text), 
		      titlefont,
		      &textcolor, 
		      &bgcolor,
		      buf, strlen(buf));
    }
    if((fd = open(file[i], O_RDONLY, "")) != -1) {
      do {
	readlen = read(fd, buf, bufsize);

	emmode = FALSE;
	modestart = buf;

	/* strip backspaced stuff */
	if(*buf == '\b')
	  *buf = ' ';
	for(j = buf, k = buf; j < buf + readlen; j++) {
	  if(*j == '\b') {
	    k--;
	    if(k != modestart)
	      gtk_text_insert(GTK_TEXT(pager_data->text), NULL, NULL, NULL,
			      modestart, k - modestart);
	    modestart = k;
	    emmode = TRUE;
	  }
	  else {
	    *k = *j;
	    k++;
	    if(emmode) {
	      gtk_text_insert(GTK_TEXT(pager_data->text),
			      emfont,
			      NULL, 
			      NULL,
			      k - 1, 1);
	      modestart = k;
	      emmode = FALSE;
	    }
	  }
	}

	gtk_text_insert(GTK_TEXT(pager_data->text), NULL, NULL, NULL,
			modestart, k - modestart);
      } while(readlen == bufsize);
    }
    else {
      g_snprintf(buf, bufsize, "NO FILE %s\n\n", file[i]);
      gtk_text_insert(GTK_TEXT(pager_data->text), NULL, NULL, NULL,
		      buf, strlen(buf));
    }
  }

  gnome_app_set_contents(GNOME_APP(pager_data->pagerwin), table);
  gtk_widget_grab_focus(pager_data->text);
  gtk_widget_show_all(pager_data->pagerwin);

  return 0;
}















