/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--1998  Robert Gentleman, Ross Ihaka and the R core team
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*
 *  SYSTEM DEPENDENT CODE
 *
 *  This source file contains the platform dependent code for
 *  the Unix (reference) port of R.
 *
 *  The first group of functions is concerned with reading and
 *  writing to the system console.
 *
 *    int   R_ReadConsole(char *prompt, char *buf, int buflen, int hist)
 *
 *  This function prints the given prompt at the console and then
 *  does a gets(3)-like operation, transfering up to "buflen" characters
 *  into the buffer "buf".  The last two characters are set to "\n\0"
 *  to preserve sanity.	 If "hist" is non-zero, then the line is added
 *  to any command history which is being maintained.  Note that this
 *  is one natural place from which to run an event loop.
 *
 *    void  R_WriteConsole(char *buf, int buflen)
 *
 *  This function writes the given buffer out to the console.  No
 *  special actions are required.  Under Unix the characters are
 *  just appended to stdout.
 *
 *    void  R_ResetConsole(void)
 *
 *  This function is called when the system is reset after an error.
 *  It probably isn't really needed.
 *
 *    void  R_FlushConsole(void)
 *
 *  This called to flush any output to the system console.  Under Unix
 *  this is just fflush(stdout).  Other systems may not need this.
 *
 *    void  R_ClearerrConsole(void)
 *
 *  This function clears any errors associated with reading from the
 *  console.  In Unix is is used to clear any EOF condition associated
 *  with stdin.
 *
 *    void  R_Suicide(char *msg)
 *
 *  This function displays the given message and the causes R to
 *  die immediately.  It is used for non-recoverable errors such as
 *  not having enough memory to launch etc.  The phrase "dialog box"
 *  springs to mind for non-unix platforms.
 *
 *    void  R_Busy(int which)
 *
 *  This function invokes actions (such as change of cursor) when
 *  R embarks on an extended computation (which=1) and when such a
 *  state terminates (which=0).
 *
 *    void  R_CleanUp(int ask)
 *
 *  This function invokes any actions which occur at system termination.
 *
 *    char* R_ExpandFileName(char *s)
 *
 *  This is a utility function which can be used to expand special
 *  characters in file names.  In Unix it's sole function is to expand
 *  and "~"s which occur in filenames (and then only when the readline
 *  library is available.  The minimal action is to return the argument
 *  unaltered.
 *
 *    void  R_InitialData(void)
 *    FILE* R_OpenInitFile(void)
 *    FILE* R_OpenLibraryFile(char *file)
 *    FILE* R_OpenSysInitFile(void)
 *
 *  The following two functions save and restore the user's global
 *  environment.  The system specific aspect of this what files
 *  are used for this.
 *
 *    void  R_RestoreGlobalEnv(void)
 *    void  R_SaveGlobalEnv(void)
 *
 *  Platform dependent functions.
 *
 *    SEXP  do_interactive(SEXP call, SEXP op, SEXP args, SEXP rho)
 *    SEXP  do_machine(SEXP call, SEXP op, SEXP args, SEXP rho)
 *    SEXP  do_proctime(SEXP call, SEXP op, SEXP args, SEXP rho)
 *    SEXP  do_quit(SEXP call, SEXP op, SEXP args, SEXP rho)
 *    SEXP  do_system(SEXP call, SEXP op, SEXP args, SEXP rho)
 */

#include "Defn.h"
#include "Fileio.h"
#include "Graphics.h"/* KillAllDevices() [nothing else?] */
#include "devGNOME.h"

#ifdef HAVE_LIBREADLINE
#include <readline/readline.h>
#ifdef HAVE_READLINE_HISTORY_H
#include <readline/history.h>
#endif
#endif

/*-- necessary for some (older, i.e., ~ <= 1997) Linuxen:*/
#ifdef linux
#ifndef FD_SET
#include <sys/time.h>
#endif
#endif

#include "terminal.h"
#include "gtkconsole.h"

#include <gnome.h>

static int UsingReadline = 1;
static int DefaultSaveAction = 0;
static int DefaultRestoreAction = 1;
static int LoadSiteFile = 1;
static int LoadInitFile = 1;
static int DebugInitFile = 0;

	/*--- I/O Support Code ---*/

	/* These routines provide hooks for supporting console I/O.
	 * Under raw Unix these routines simply provide a
	 * connection to the stdio library.
	 * Under a Motif interface the routines would be
	 * considerably more complex.
	 */


/*
  Catch input in the console window
*/
void R_gtk_terminal_line_event(GtkWidget *widget)
{
  gtk_main_quit();
}

	/* Fill a text buffer with user typed console input. */

int R_ReadConsole(char *prompt, char *buf, int len, int addtohistory)
{
    if(!R_Interactive) {
	if(!R_Quiet) fputs(prompt, stdout);
	if (fgets(buf, len, stdin) == NULL)
	    return 0;
	if(!R_Quiet) fputs(buf,stdout);
	return 1;
    }
    else {
      gtk_console_enable_input(GTK_CONSOLE(R_gtk_terminal_text), prompt, strlen(prompt));
      gtk_signal_connect(GTK_OBJECT(R_gtk_terminal_text),
			 "console_line_ready",
			 GTK_SIGNAL_FUNC(R_gtk_terminal_line_event),
			 NULL);

      gtk_main();

      gtk_console_read(GTK_CONSOLE(R_gtk_terminal_text), buf, len, addtohistory);

    }
}

	/* Write a text buffer to the console. */
	/* All system output is filtered through this routine. */
void R_WriteConsole(char *buf, int len)
{
  gtk_console_write(GTK_CONSOLE(R_gtk_terminal_text), buf, len);
}


	/* Indicate that input is coming from the console */

void R_ResetConsole()
{
}


	/* Stdio support to ensure the console file buffer is flushed */

void R_FlushConsole()
{
  gtk_console_flush(GTK_CONSOLE(R_gtk_terminal_text));
}


	/* Reset stdin if the user types EOF on the console. */

void R_ClearerrConsole()
{
}


	/*--- File Handling Code ---*/


#ifdef HAVE_LIBREADLINE
char *tilde_expand(char*);

char *R_ExpandFileName(char *s)
{
    return tilde_expand(s);
}
#else
char *R_ExpandFileName(char *s)
{
    return s;
}
#endif

FILE *R_fopen(const char *filename, const char *mode)
{
	return( fopen(filename, mode) );
}

FILE *R_OpenLibraryFile(char *file)
{
    char buf[256], *rhome;
    FILE *fp;

    if((rhome = getenv("RHOME")) == NULL)
	return NULL;
    sprintf(buf, "%s/library/base/R/%s", rhome, file);
    fp = R_fopen(buf, "r");
    return fp;
}

FILE *R_OpenSysInitFile(void)
{
    char buf[256], *rhome;
    FILE *fp;

    if((rhome = getenv("RHOME")) == NULL)
	return NULL;
    sprintf(buf, "%s/library/base/R/Rprofile", rhome);
    fp = R_fopen(buf, "r");
    return fp;
}

FILE *R_OpenSiteFile(void)
{
    char buf[256], *rhome;
    FILE *fp;

    fp = NULL;

    if (LoadSiteFile) {
	if ((fp = R_fopen(getenv("RPROFILE"), "r")))
	    return fp;
	if ((rhome = getenv("RHOME")) == NULL)
	    return NULL;
	sprintf(buf, "%s/etc/Rprofile", rhome);
	if ((fp = R_fopen(buf, "r")))
	    return fp;
    }

    return fp;
}

FILE *R_OpenInitFile(void)
{
    char buf[256], *home;
    FILE *fp;

    fp = NULL;

    if (LoadInitFile) {
	if ((fp = R_fopen(".Rprofile", "r")))
	    return fp;
	if ((home = getenv("HOME")) == NULL)
	    return NULL;
	sprintf(buf, "%s/.Rprofile", home);
	if ((fp = R_fopen(buf, "r")))
	    return fp;
    }

    return fp;
}


	/*--- Initialization Code ---*/

#ifdef HAVE_TIMES
#include <sys/times.h>

static clock_t StartTime;
static struct tms timeinfo;
#endif

#ifdef __FreeBSD__
#include <floatingpoint.h>
#endif

#ifdef linux
#include <fpu_control.h>
#endif

int main(int ac, char **av)
{
    int value;
    char *p;

    gnome_init("R.gnome", g_strdup_printf("%s.%s %s (%s %s, %s)", R_MAJOR, R_MINOR, R_STATUS, R_MONTH, R_DAY, R_YEAR), ac, av);

    R_gnome_load_prefs();

    gc_inhibit_torture = 1;
#ifdef HAVE_TIMES
    StartTime = times(&timeinfo);
#endif
    R_Quiet = 0;

    while(--ac) {
	if(**++av == '-') {
	    if (!strcmp(*av, "--version")) {
		Rprintf("Version %s.%s %s (%s %s, %s)\n",
			R_MAJOR, R_MINOR, R_STATUS, R_MONTH, R_DAY, R_YEAR);
		Rprintf("Copyright (C) %s R Core Team\n\n", R_YEAR);
		Rprintf("R is free software and comes with ABSOLUTELY NO WARRANTY.\n");
		Rprintf("You are welcome to redistribute it under the terms of the\n");
		Rprintf("GNU General Public License.  For more information about\n");
		Rprintf("these matters, see http://www.gnu.org/copyleft/gpl.html.\n");
		exit(0);
	    }
	    else if(!strcmp(*av, "--save")) {
		DefaultSaveAction = 3;
	    }
	    else if(!strcmp(*av, "--no-save")) {
		DefaultSaveAction = 2;
	    }
	    else if(!strcmp(*av, "--restore")) {
		DefaultRestoreAction = 1;
	    }
	    else if(!strcmp(*av, "--no-restore")) {
		DefaultRestoreAction = 0;
	    }
	    else if(!strcmp(*av, "--no-readline")) {
		UsingReadline = 0;
	    }
	    else if (!strcmp(*av, "--silent") ||
		     !strcmp(*av, "--quiet") ||
		     !strcmp(*av, "-q")) {
		R_Quiet = 1;
	    }
	    else if (!strcmp(*av, "--vanilla")) {
		DefaultSaveAction = 2;/* --no-save */
		DefaultRestoreAction = 0;/* --no-restore */
		LoadSiteFile = 0;/* --no-site-file */
		LoadInitFile = 0;/* --no-init-file */
	    }
	    else if (!strcmp(*av, "--verbose")) {
		R_Verbose = 1;
	    }
	    else if (!strcmp(*av, "--slave") ||
		     !strcmp(*av, "-s")) {
		R_Quiet = 1;
		R_Slave = 1;
		DefaultSaveAction = 2;
	    }
	    else if (!strcmp(*av, "--no-site-file")) {
		LoadSiteFile = 0;
	    }
	    else if (!strcmp(*av, "--no-init-file")) {
		LoadInitFile = 0;
	    }
	    else if (!strcmp(*av, "--debug-init")) {
		DebugInitFile = 1;
	    }
	    else if (!strcmp(*av, "-save") ||
		     !strcmp(*av, "-nosave") ||
		     !strcmp(*av, "-restore") ||
		     !strcmp(*av, "-norestore") ||
		     !strcmp(*av, "-noreadline") ||
		     !strcmp(*av, "-quiet") ||
		     !strcmp(*av, "-V")) {
		REprintf("WARNING: option %s no longer supported\n", *av);
	    }
	    else if((*av)[1] == 'v') {
		REprintf("WARNING: option `-v' is deprecated.  ");
		REprintf("Use `--vsize' instead.\n");
		if((*av)[2] == '\0') {
		    ac--; av++; p = *av;
		}
		else p = &(*av)[2];
		value = strtol(p, &p, 10);
		if(*p) goto badargs;
		if(value < 1 || value > 1000)
		    REprintf("WARNING: invalid vector heap size ignored\n");
		else
		    R_VSize = value * 1048576; /* 1 MByte := 2^20 Bytes*/
	    }
	    else if (!strcmp(*av, "--vsize")) {
		ac--; av++; p = *av;
		value = strtol(p, &p, 10);
		if(*p) goto badargs;
		if(value < 1 || value > 1000)
		    REprintf("WARNING: invalid vector heap size '%d' ignored, using default = %g\n", value, R_VSize / 1048576.0);
		else
		    R_VSize = value * 1048576; /* 1 MByte := 2^20 Bytes*/
	    }
	    else if((*av)[1] == 'n') {
		REprintf("WARNING: option `-n' is deprecated.  ");
		REprintf("Use `--nsize' instead.\n");
		if((*av)[2] == '\0') {
		    ac--; av++; p = *av;
		}
		else p = &(*av)[2];
		value = strtol(p, &p, 10);
		if(*p) goto badargs;
		if(value < R_NSize || value > 1000000)
		    REprintf("WARNING: invalid language heap size ignored\n");
		else
		    R_NSize = value;
	    }
	    else if (!strcmp(*av, "--nsize")) {
		ac--; av++; p = *av;
		value = strtol(p, &p, 10);
		if(*p) goto badargs;
		if(value < R_NSize || value > 1000000)
		    REprintf("WARNING: invalid language heap size '%d' ignored, using default = %d\n", value, R_NSize);
		else
		    R_NSize = value;
	    }
	    else {
		REprintf("WARNING: unknown option %s\n", *av);
		break;
	    }
	}
	else {
	    printf("ARGUMENT '%s' __ignored__\n", *av);
	}
    }

    /* On Unix the console is a file; we just use stdio to write on it */

    R_Interactive = 1;
    R_Sinkfile = NULL;

    if(!R_Interactive && DefaultSaveAction == 0)
	R_Suicide("you must specify `--save' or `--no-save'");

#ifdef __FreeBSD__
    fpsetmask(0);
#endif

#ifdef linux
    __setfpucw(_FPU_IEEE);
#endif

    R_gtk_terminal_new();

    gtk_console_restore_history(GTK_CONSOLE(R_gtk_terminal_text), ".Rgnomehistory", 50, NULL);

    mainloop();
    /*++++++  in ../main/main.c */

    return 0;

badargs:
    REprintf("invalid argument passed to R\n");
    exit(1);
}

void R_InitialData(void)
{
    R_RestoreGlobalEnv();
}

	/* R_CleanUp is invoked at the end of the session to give */
	/* the user the option of saving their data.  If ask=1 the */
	/* user is asked their preference, if ask=2 the answer is */
	/* assumed to be "no" and if ask=3 the answer is assumed to */
	/* be "yes".  When R is being used non-interactively, and */
	/* ask=1, the value is changed to 3.  The philosophy is */
	/* that saving unwanted data is less bad than non saving */
	/* data that is wanted. */

void R_CleanUp(int ask)
{
    GtkWidget *dialog;
    gint which; // yes = 0, no = 1, cancel = 2 || -1

    GList *curfile = R_gtk_editfiles;
    R_gtk_edititem *edititem;

    if( R_DirtyImage ) {
    qask:
	R_ClearerrConsole();
	R_FlushConsole();
	if(!R_Interactive && ask==1)
	    ask = DefaultSaveAction;

	if(ask == 1) {
	  dialog = gnome_message_box_new("Do you want to save your workspace image?\n\n\
Choose Yes to save an image and exit,\nchoose No to exit without saving,\nor choose Cancel to return to R.",
					 GNOME_MESSAGE_BOX_QUESTION,
					 GNOME_STOCK_BUTTON_YES,
					 GNOME_STOCK_BUTTON_NO,
					 GNOME_STOCK_BUTTON_CANCEL,
					 NULL);

	  gnome_dialog_set_parent(GNOME_DIALOG(dialog), GTK_WINDOW(R_gtk_main_window));
	  gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
	  gnome_dialog_set_default(GNOME_DIALOG(dialog), 0);
	  
	  which = gnome_dialog_run_and_close(GNOME_DIALOG(dialog));

	}
	else if(ask == 2)
	    which = 1;
	else if (ask == 3)
	    which = 0;

	switch (which) {
	case 0:
	    R_SaveGlobalEnv();

	    if(R_Interactive)
	      gtk_console_save_history(GTK_CONSOLE(R_gtk_terminal_text), ".Rgnomehistory", 50, NULL);
	    break;
	case 1:
	    break;
	default:
	    jump_to_toplevel();
	    break;
	}
    }

    /* close all the graphics devices */
    KillAllDevices();

    /* unlink all the files we opened for editing */
    while(curfile != NULL) {
      edititem = (R_gtk_edititem *) curfile->data;
      unlink(edititem->filename);
      curfile = g_list_next(curfile);
    }

#ifdef __FreeBSD__
    fpsetmask(~0);
#endif

#ifdef linux
    __setfpucw(_FPU_DEFAULT);
#endif

    exit(0);
}

void R_Busy(int which)
{
}

	/* Saving and Restoring the Global Environment */

void R_SaveGlobalEnv(void)
{
    FILE *fp = R_fopen(".RData", "w");
    if (!fp)
	error("can't save data -- unable to open ./.RData\n");
    R_SaveToFile(FRAME(R_GlobalEnv), fp, 0);
    fclose(fp);
}

void R_RestoreGlobalEnv(void)
{
    FILE *fp;
    if(DefaultRestoreAction) {
	if(!(fp = R_fopen(".RData","r"))) {
	    /* warning here perhaps */
	    return;
	}
	FRAME(R_GlobalEnv) = R_LoadFromFile(fp);
	if(!R_Quiet)
	    Rprintf("[Previously saved workspace restored]\n\n");
    }
}


	/*--- Platform Dependent Functions ---*/


#ifdef HAVE_TIMES
#ifndef CLK_TCK
/* this is in ticks/second, generally 60 on BSD style Unix, 100? on SysV */
#ifdef HZ
#define CLK_TCK HZ
#else
#define CLK_TCK	60
#endif

#endif



SEXP do_proctime(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    clock_t elapsed;
    elapsed = (times(&timeinfo) - StartTime) / (double)CLK_TCK;
    ans = allocVector(REALSXP, 5);
    REAL(ans)[0] = timeinfo.tms_utime / (double)CLK_TCK;
    REAL(ans)[1] = timeinfo.tms_stime / (double)CLK_TCK;
    REAL(ans)[2] = elapsed;
    REAL(ans)[3] = timeinfo.tms_cutime / (double)CLK_TCK;
    REAL(ans)[4] = timeinfo.tms_cstime / (double)CLK_TCK;
    return ans;
}
#endif
extern char ** environ;

SEXP do_getenv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, j;
    char *s;
    char **e;
    SEXP ans;

    checkArity(op, args);

    if(!isString(CAR(args)))
	errorcall(call, "wrong type for argument\n");

    i = LENGTH(CAR(args));
    if (i == 0) {
	for (i = 0, e = environ; *e != NULL; i++, e++);
	PROTECT(ans = allocVector(STRSXP, i));
	for (i = 0, e = environ; *e != NULL; i++, e++)
	    STRING(ans)[i] = mkChar(*e);
    } else {
	PROTECT(ans = allocVector(STRSXP,i));
	for (j = 0; j < i; j++) {
	    s = getenv(CHAR(STRING(CAR(args))[j]));
	    if (s == NULL)
		STRING(ans)[j] = mkChar("");
	    else
		STRING(ans)[j] = mkChar(s);
	}
    }
    UNPROTECT(1);
    return(ans);
}

SEXP do_machine(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return mkString("Unix");
}

SEXP do_system(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    FILE *fp;
    char *x = "r", buf[120];
    int read=0, i, j;
    SEXP tlist = R_NilValue, tchar, rval;

    checkArity(op, args);
    if (!isString(CAR(args)))
	errorcall(call, "character argument expected\n");
    if (isLogical(CADR(args)))
	read = INTEGER(CADR(args))[0];
    if (read) {
	PROTECT(tlist);
	fp = popen(CHAR(STRING(CAR(args))[0]), x);
	for (i = 0; fgets(buf, 120, fp); i++) {
	    read = strlen(buf);
	    buf[read - 1] = '\0';
	    tchar = mkChar(buf);
	    UNPROTECT(1);
	    PROTECT(tlist = CONS(tchar, tlist));
	}
	pclose(fp);
	rval = allocVector(STRSXP, i);;
	for (j = (i - 1); j >= 0; j--) {
	    STRING(rval)[j] = CAR(tlist);
	    tlist = CDR(tlist);
	}
	UNPROTECT(1);
	return (rval);
    }
    else {
	tlist = allocVector(INTSXP, 1);
	fflush(stdout);
	INTEGER(tlist)[0] = system(CHAR(STRING(CAR(args))[0]));
	R_Visible = 0;
	return tlist;
    }
}

SEXP do_interactive(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rval;

    rval=allocVector(LGLSXP, 1);
    if( R_Interactive )
	LOGICAL(rval)[0]=1;
    else
	LOGICAL(rval)[0]=0;
    return rval;
}

SEXP do_quit(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    char *tmp;
    int ask=0;

    if(R_BrowseLevel) {
	warning("can't quit from browser\n");
	return R_NilValue;
    }
    if( !isString(CAR(args)) )
	errorcall(call,"one of \"yes\", \"no\" or \"ask\" expected.\n");
    tmp = CHAR(STRING(CAR(args))[0]);
    if( !strcmp(tmp,"ask") )
	ask=1;
    else if( !strcmp(tmp,"no") )
	ask=2;
    else if( !strcmp(tmp,"yes") )
	ask=3;
    else
	errorcall(call,"unrecognized value of ask\n");
    R_CleanUp(ask);
    exit(0);
    /*NOTREACHED*/
}

void suicide_delete_event(GtkWidget *widget, GdkEvent *event, gpointer data)
{
  gtk_main_quit();
}

void R_Suicide(char *s)
{
  GtkWidget *dialog;
  GtkWidget *vbox;
  GtkWidget *errortext;
  GtkWidget *hbox;
  GtkWidget *button;
  
  dialog = gtk_window_new(GTK_WINDOW_DIALOG);

  gtk_window_set_title(GTK_WINDOW(dialog), "R: Fatal error");
  gtk_widget_set_usize(dialog, 300, 150);
  gtk_container_border_width(GTK_CONTAINER(dialog), 5);
  gtk_widget_realize(dialog);

  vbox = gtk_vbox_new(FALSE, 10);
  gtk_container_add(GTK_CONTAINER(dialog), vbox);

  errortext = gtk_text_new(NULL, NULL);
  gtk_text_insert(GTK_TEXT(errortext), NULL, NULL, NULL, s, strlen(s));
  gtk_box_pack_start(GTK_BOX(vbox), errortext, TRUE, FALSE, 0);

  hbox = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, FALSE, 0);

  button = gtk_button_new_with_label("OK");
  gtk_box_pack_start(GTK_BOX(hbox), button, TRUE, FALSE, 0);

  gtk_widget_show_all(dialog);

  gtk_signal_connect(GTK_OBJECT(dialog), "delete_event", GTK_SIGNAL_FUNC(suicide_delete_event), NULL);
  gtk_signal_connect(GTK_OBJECT(dialog), "destroy", GTK_SIGNAL_FUNC(suicide_delete_event), NULL);
  gtk_signal_connect(GTK_OBJECT(button), "clicked", GTK_SIGNAL_FUNC(suicide_delete_event), NULL);
  
  gtk_main();

    R_CleanUp(2);
    /*	 2 means don't save anything and it's an unrecoverable abort */
}


	/* Declarations to keep f77 happy */

int MAIN_()  {return 0;}
int MAIN__() {return 0;}
int __main() {return 0;}



/* New / Experimental API elements */

/* Local Time and Date */

/* If the time/ctime version fails */
/* use the follwing */

#ifdef FALLBACK
static char datebuf[64];
char *R_Date()
{
    FILE *fp;
    char *p;
    if ((fp = popen("date", "r")) == NULL)
	error("unix pipe error in date function\n");
    fgets(datebuf, 64, fp);
    fclose(fp);
    for (p = datebuf; *p ; p++)
	if (*p == '\n') {
	    *p = '\0';
	    break;
	}
    return datebuf;
}
#else
#include <time.h>

char *R_Date()
{
    time_t t;
    time(&t);
    return ctime(&t);
}
#endif

void R_ShowFile(char *file, char *title)
{
    FILE *fp;
    int c;
    if ((fp = fopen(file, "r")) == NULL)
	error("unable to display file %s\n", file);
    while ((c = getc(fp)) != EOF)
      /* FIXME: do something here */;
    fclose(fp);
}
