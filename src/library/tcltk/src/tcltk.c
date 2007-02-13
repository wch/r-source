
#include "tcltk.h" /* declarations of our `public' interface */
#ifndef Win32
#include <R_ext/eventloop.h>
#endif

#ifndef TCL80
# define SUPPORT_MBCS 1
/* Includes Nakama's internationalization patches */
#endif

#include <R.h>

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("tcltk", String)
#else
#define _(String) (String)
#endif

extern int (*R_timeout_handler)();
extern long R_timeout_val;

static Tcl_Interp *RTcl_interp;      /* Interpreter for this application. */

#ifndef Win32
/* R event structure */
typedef struct {
    Tcl_EventProc *proc;
    struct Tcl_Event *nextPtr;
} RTcl_Event;
#endif

static void RTcl_dec_refcount(SEXP R_tclobj)
{
    Tcl_DecrRefCount((Tcl_Obj *) R_ExternalPtrAddr(R_tclobj));
}

static SEXP makeRTclObject(Tcl_Obj *tclobj)
{
    SEXP obj;

    obj = R_MakeExternalPtr(tclobj, R_NilValue, R_NilValue);
    Tcl_IncrRefCount(tclobj);
    R_RegisterCFinalizer(obj, RTcl_dec_refcount);
    return obj;
}

static int R_eval(ClientData clientData,
		  Tcl_Interp *interp,
		  int argc,
		  CONST84 char *argv[])
{
    ParseStatus status;
    int i;
    SEXP text, expr, ans=R_NilValue /* -Wall */;

    text = PROTECT(allocVector(STRSXP, argc - 1));
    for (i = 1 ; i < argc ; i++)
	SET_STRING_ELT(text, i-1, mkChar(argv[i]));

    expr = PROTECT(R_ParseVector(text, -1, &status, R_NilValue));
    if (status != PARSE_OK) {
	UNPROTECT(2);
	Tcl_SetResult(interp, _("parse error in R expression"), TCL_STATIC);
	return TCL_ERROR;
    }

    /* Note that expr becomes an EXPRSXP and hence we need the loop
       below (a straight eval(expr, R_GlobalEnv) won't work) */
    {
	int n = length(expr);
	for(i = 0 ; i < n ; i++)
	    ans = eval(VECTOR_ELT(expr, i), R_GlobalEnv);
    }

    /* If return value is of class tclObj, use as Tcl result */
    if (inherits(ans, "tclObj"))
	    Tcl_SetObjResult(interp, (Tcl_Obj*) R_ExternalPtrAddr(ans));

    UNPROTECT(2);
    return TCL_OK;
}

/* Functions to evaluate callbacks. Notice that these have no error
   checks, since the calls are assumed to be generated internally, and
   not by users. There are two forms:

   R_call 0xnnnnnnnn arg1 arg2 arg3...
   R_call_lang 0xnnnnnnnn 0xmmmmmmmm

   In the former, the hex address is assumed to correspond to a
   function closure which gets called with the arguments given.  The
   latter assumes that the first hex address is a LANGSXP and the
   second is an ENVSXP. */

static int R_call(ClientData clientData,
		  Tcl_Interp *interp,
		  int argc,
		  CONST84 char *argv[])
{
    int i;
    SEXP expr, alist, ans;
    void *fun;

    alist = R_NilValue;
    for (i = argc - 1 ; i > 1 ; i--){
	PROTECT(alist);
	alist = LCONS(mkString(argv[i]), alist);
	UNPROTECT(1);
    }

    sscanf(argv[1], "%p", &fun);

    expr = LCONS( (SEXP)fun, alist);
    expr = LCONS(install("try"), LCONS(expr, R_NilValue));

    ans = eval(expr, R_GlobalEnv);

    /* If return value is of class tclObj, use as Tcl result */
    if (inherits(ans, "tclObj"))
	Tcl_SetObjResult(interp, (Tcl_Obj*) R_ExternalPtrAddr(ans));

    return TCL_OK;
}

static int R_call_lang(ClientData clientData,
		       Tcl_Interp *interp,
		       int argc,
		       CONST84 char *argv[])
{
    void *expr, *env; 
    SEXP ans;

    sscanf(argv[1], "%p", &expr);
    sscanf(argv[2], "%p", &env);

    expr = LCONS(install("try"), LCONS(expr, R_NilValue));

    ans = eval((SEXP)expr, (SEXP)env);

    /* If return value is of class tclObj, use as Tcl result */
    if (inherits(ans, "tclObj"))
	Tcl_SetObjResult(interp, (Tcl_Obj*) R_ExternalPtrAddr(ans));

    return TCL_OK;
}


static Tcl_Obj * tk_eval(char *cmd)
{
#ifdef SUPPORT_MBCS
    char *cmd_utf8;
    Tcl_DString  cmd_utf8_ds;

    Tcl_DStringInit(&cmd_utf8_ds);
    cmd_utf8 = Tcl_ExternalToUtfDString(NULL, cmd, -1, &cmd_utf8_ds);
    if (Tcl_Eval(RTcl_interp, cmd_utf8) == TCL_ERROR)
#else
    if (Tcl_Eval(RTcl_interp, cmd) == TCL_ERROR)
#endif
    {
	char p[512];
	if (strlen(Tcl_GetStringResult(RTcl_interp)) > 500)
	    strcpy(p, _("tcl error.\n"));
	else
#ifdef SUPPORT_MBCS
	{
	    char *res;
	    Tcl_DString  res_ds;

	    Tcl_DStringInit(&res_ds);
	    res = Tcl_UtfToExternalDString(NULL,
					   Tcl_GetStringResult(RTcl_interp),
					   -1, &res_ds);
	    snprintf(p, sizeof(p), "[tcl] %s.\n", res);
	    Tcl_DStringFree(&res_ds);
	}
#else
            snprintf(p, sizeof(p), "[tcl] %s.\n",
		     Tcl_GetStringResult(RTcl_interp));
#endif
	error(p);
    }
#ifdef SUPPORT_MBCS
    Tcl_DStringFree(&cmd_utf8_ds);
#endif
    return Tcl_GetObjResult(RTcl_interp);
}

/* FIXME get rid of fixed size buffers in do_Tclcallback et al. (low
   priority, since there is no meaningful reason to exceed the current
   buffer length) */

SEXP dotTcl(SEXP args)
{
    SEXP ans;
    char *cmd;
    Tcl_Obj *val;
    if(!isValidString(CADR(args)))
	error(_("invalid argument"));
    cmd = translateChar(STRING_ELT(CADR(args), 0));
    val = tk_eval(cmd);
    ans = makeRTclObject(val);
    return ans;
}

SEXP dotTclObjv(SEXP args)
{
    SEXP t,
	avec = CADR(args),
	nm = getAttrib(avec, R_NamesSymbol);
    int objc, i, result;
    Tcl_Obj **objv;

    for (objc = 0, i = 0; i < length(avec); i++){
	if (!isNull(VECTOR_ELT(avec, i)))
	    objc++;
	if (!isNull(nm) && strlen(translateChar(STRING_ELT(nm, i))))
	    objc++;
    }

    objv = (Tcl_Obj **) R_alloc(objc, sizeof(Tcl_Obj *));

    for (objc = i = 0; i < length(avec); i++){
	char *s, *tmp;
	if (!isNull(nm) && strlen(s = translateChar(STRING_ELT(nm, i)))){
	    tmp = calloc(strlen(s)+2, sizeof(char));
	    *tmp = '-';
	    strcpy(tmp+1, s);
	    objv[objc++] = Tcl_NewStringObj(tmp, -1);
	    free(tmp);
	}
	if (!isNull(t = VECTOR_ELT(avec, i)))
	    objv[objc++] = (Tcl_Obj *) R_ExternalPtrAddr(t);
    }

    for (i = objc; i--; ) Tcl_IncrRefCount(objv[i]);
    result = Tcl_EvalObjv(RTcl_interp, objc, objv, 0);
    for (i = objc; i--; ) Tcl_DecrRefCount(objv[i]);

    if (result == TCL_ERROR)
    {
	char p[512];
	if (strlen(Tcl_GetStringResult(RTcl_interp)) > 500)
	    strcpy(p, _("tcl error.\n"));
	else
#ifdef SUPPORT_MBCS
	{
	    char *res;
	    Tcl_DString  res_ds;
	    Tcl_DStringInit(&res_ds);
	    res = Tcl_UtfToExternalDString(NULL,
					   Tcl_GetStringResult(RTcl_interp),
					   -1, &res_ds);
	    snprintf(p, sizeof(p), "[tcl] %s.\n", res);
	    Tcl_DStringFree(&res_ds);
	}
#else
	    snprintf(p, sizeof(p), "[tcl] %s.\n",
		     Tcl_GetStringResult(RTcl_interp));
#endif
	error(p);
    }

    return makeRTclObject(Tcl_GetObjResult(RTcl_interp));
}


SEXP RTcl_ObjFromVar(SEXP args)
{
    Tcl_Obj *tclobj;

#ifndef TCL80
    tclobj = Tcl_GetVar2Ex(RTcl_interp,
                           translateChar(STRING_ELT(CADR(args), 0)),
                           NULL,
                           0);
#else
    Tcl_Obj *tclname;

    tclname = Tcl_NewStringObj(translateChar(STRING_ELT(CADR(args), 0)), -1);
    tclobj = Tcl_ObjGetVar2(RTcl_interp,
                           tclname,
                           NULL,
                           0);
    Tcl_DecrRefCount(tclname);
#endif
    return makeRTclObject(tclobj);
}

SEXP RTcl_AssignObjToVar(SEXP args)
{
    Tcl_Obj *tclobj;

#ifndef TCL80
    tclobj = Tcl_SetVar2Ex(RTcl_interp,
                           translateChar(STRING_ELT(CADR(args), 0)),
                           NULL,
                           (Tcl_Obj *) R_ExternalPtrAddr(CADDR(args)),
                           0);
#else
    Tcl_Obj *tclname;

    tclname = Tcl_NewStringObj(translateChar(STRING_ELT(CADR(args), 0)), -1);
    tclobj = Tcl_ObjSetVar2(RTcl_interp,
                           tclname,
                           NULL,
                           (Tcl_Obj *) R_ExternalPtrAddr(CADDR(args)),
                           0);
    Tcl_DecrRefCount(tclname);
#endif

    return R_NilValue;
}


SEXP RTcl_StringFromObj(SEXP args)
{
    char *str;
#ifdef SUPPORT_MBCS
    SEXP so;
    char *s;
    Tcl_DString s_ds;

    Tcl_DStringInit(&s_ds);
    str = Tcl_GetStringFromObj((Tcl_Obj *) R_ExternalPtrAddr(CADR(args)),
			       NULL);
    s = Tcl_UtfToExternalDString(NULL, str, -1, &s_ds);
    so = mkString(s);
    Tcl_DStringFree(&s_ds);
    return(so);
#else

    str = Tcl_GetStringFromObj((Tcl_Obj *) R_ExternalPtrAddr(CADR(args)),
			       NULL);
    return mkString(str);
#endif
}

SEXP RTcl_ObjAsCharVector(SEXP args)
{
    int count;
    Tcl_Obj **elem;
    int ret, i;
    SEXP ans;

    ret = Tcl_ListObjGetElements(RTcl_interp,
				 (Tcl_Obj *) R_ExternalPtrAddr(CADR(args)),
				 &count, &elem);
    if (ret != TCL_OK)
	return RTcl_StringFromObj(args);

    PROTECT(ans = allocVector(STRSXP, count));
    for (i = 0 ; i < count ; i++)
#ifdef SUPPORT_MBCS
    {
	char *s;
	Tcl_DString s_ds;
	Tcl_DStringInit(&s_ds);
	s = Tcl_UtfToExternalDString(NULL,
				     (Tcl_GetStringFromObj(elem[i], NULL)),
				     -1, &s_ds);
	SET_STRING_ELT(ans, i, mkChar(s));
	Tcl_DStringFree(&s_ds);
    }
#else
	SET_STRING_ELT(ans, i, mkChar(Tcl_GetStringFromObj(elem[i], NULL)));
#endif
    UNPROTECT(1);
    return ans;
}

SEXP RTcl_ObjFromCharVector(SEXP args)
{
#ifdef SUPPORT_MBCS
    char *s;
    Tcl_DString s_ds;
#endif
    int count;
    Tcl_Obj *tclobj, *elem;
    int i;
    SEXP val, drop;

    val = CADR(args);
    drop = CADDR(args);

    tclobj = Tcl_NewObj();

    count = length(val);
    if (count == 1 && LOGICAL(drop)[0])
#ifdef SUPPORT_MBCS
    {
	Tcl_DStringInit(&s_ds);
	s = Tcl_ExternalToUtfDString(NULL,
				     translateChar(STRING_ELT(val, 0)), -1, &s_ds);
	Tcl_SetStringObj(tclobj, s, -1);
	Tcl_DStringFree(&s_ds);
    }
#else
	Tcl_SetStringObj(tclobj, translateChar(STRING_ELT(val, 0)), -1);
#endif
    else
	for ( i = 0 ; i < count ; i++) {
	    elem = Tcl_NewObj();
#ifdef SUPPORT_MBCS
	    Tcl_DStringInit(&s_ds);
	    s = Tcl_ExternalToUtfDString(NULL, translateChar(STRING_ELT(val, i)),
					 -1, &s_ds);
	    Tcl_SetStringObj(elem, s, -1);
	    Tcl_DStringFree(&s_ds);
#else
	    Tcl_SetStringObj(elem, translateChar(STRING_ELT(val, i)), -1);
#endif
	    Tcl_ListObjAppendElement(RTcl_interp, tclobj, elem);
	}

    return makeRTclObject(tclobj);
}

SEXP RTcl_ObjAsDoubleVector(SEXP args)
{
    int count;
    Tcl_Obj **elem, *obj;
    int ret, i;
    double x;
    SEXP ans;

    obj = (Tcl_Obj *) R_ExternalPtrAddr(CADR(args));

    /* First try for single value */
    ret = Tcl_GetDoubleFromObj(RTcl_interp, obj, &x);
    if (ret == TCL_OK) {
	ans = allocVector(REALSXP, 1);
	REAL(ans)[0] = x;
	return ans;
    }

    /* Then try as list */
    ret = Tcl_ListObjGetElements(RTcl_interp, obj, &count, &elem);
    if (ret != TCL_OK) /* didn't work, return NULL */
	return R_NilValue;

    ans = allocVector(REALSXP, count);
    for (i = 0 ; i < count ; i++){
	ret = Tcl_GetDoubleFromObj(RTcl_interp, elem[i], &x);
	if (ret != TCL_OK) x = NA_REAL;
	REAL(ans)[i] = x;
    }
    return ans;
}

static Tcl_Obj *NewIntOrDoubleObj(double x)
{
    /* This function works around two quirks: (1) that numeric values
       in R are generally stored as doubles, even small integer
       constants and (2) that Tcl stringifies a double constant like 2
       into the form 2.0, which will not work ins some connections */
    int i = (int) x;
    return ((double) i == x) ? Tcl_NewIntObj(i) : Tcl_NewDoubleObj(x);
}

SEXP RTcl_ObjFromDoubleVector(SEXP args)
{
    int count;
    Tcl_Obj *tclobj, *elem;
    int i;
    SEXP val, drop;

    val = CADR(args);
    drop = CADDR(args);

    tclobj = Tcl_NewObj();

    count = length(val);
    if (count == 1 && LOGICAL(drop)[0])
	tclobj = NewIntOrDoubleObj(REAL(val)[0]);
    else
	for ( i = 0 ; i < count ; i++) {
	    elem = NewIntOrDoubleObj(REAL(val)[i]);
	    Tcl_ListObjAppendElement(RTcl_interp, tclobj, elem);
	}

    return makeRTclObject(tclobj);
}

SEXP RTcl_ObjAsIntVector(SEXP args)
{
    int count;
    Tcl_Obj **elem, *obj;
    int ret, i;
    int x;
    SEXP ans;

    obj = (Tcl_Obj *) R_ExternalPtrAddr(CADR(args));

    /* First try for single value */
    ret = Tcl_GetIntFromObj(RTcl_interp, obj, &x);
    if (ret == TCL_OK) {
	ans = allocVector(INTSXP, 1);
	INTEGER(ans)[0] = x;
	return ans;
    }

    /* Then try as list */
    ret = Tcl_ListObjGetElements(RTcl_interp, obj, &count, &elem);
    if (ret != TCL_OK) /* didn't work, return NULL */
	return R_NilValue;

    ans = allocVector(INTSXP, count);
    for (i = 0 ; i < count ; i++){
	ret = Tcl_GetIntFromObj(RTcl_interp, elem[i], &x);
	if (ret != TCL_OK) x = NA_REAL;
	INTEGER(ans)[i] = x;
    }
    return ans;
}

SEXP RTcl_ObjFromIntVector(SEXP args)
{
    int count;
    Tcl_Obj *tclobj, *elem;
    int i;
    SEXP val, drop;

    val = CADR(args);
    drop = CADDR(args);

    tclobj = Tcl_NewObj();

    count = length(val);
    if (count == 1 && LOGICAL(drop)[0])
	tclobj = Tcl_NewIntObj(INTEGER(val)[0]);
    else
	for ( i = 0 ; i < count ; i++) {
	    elem = Tcl_NewIntObj(INTEGER(val)[i]);
	    Tcl_ListObjAppendElement(RTcl_interp, tclobj, elem);
	}

    return makeRTclObject(tclobj);
}

#ifndef TCL80
SEXP RTcl_GetArrayElem(SEXP args)
{
    SEXP x, i;
    char *xstr, *istr;
    Tcl_Obj *tclobj;

    x = CADR(args);
    i = CADDR(args);

    xstr = translateChar(STRING_ELT(x, 0));
    istr = translateChar(STRING_ELT(i, 0));
    tclobj = Tcl_GetVar2Ex(RTcl_interp, xstr, istr, 0);

    if (tclobj == NULL)
	return R_NilValue;
    else
	return makeRTclObject(tclobj);
}

SEXP RTcl_SetArrayElem(SEXP args)
{
    SEXP x, i;
    char *xstr, *istr;
    Tcl_Obj *value;

    x = CADR(args);
    i = CADDR(args);
    value = (Tcl_Obj *) R_ExternalPtrAddr(CADDDR(args));

    xstr = translateChar(STRING_ELT(x, 0));
    istr = translateChar(STRING_ELT(i, 0));
    Tcl_SetVar2Ex(RTcl_interp, xstr, istr, value, 0);

    return R_NilValue;
}

SEXP RTcl_RemoveArrayElem(SEXP args)
{
    SEXP x, i;
    char *xstr, *istr;

    x = CADR(args);
    i = CADDR(args);

    xstr = translateChar(STRING_ELT(x, 0));
    istr = translateChar(STRING_ELT(i, 0));
    Tcl_UnsetVar2(RTcl_interp, xstr, istr, 0);

    return R_NilValue;
}
#endif /* TCL80 */

static void callback_closure(char * buf, int buflen, SEXP closure)
{
    static char tmp[21];
    SEXP formals;

    formals = FORMALS(closure);

    sprintf(buf, "R_call %p", (void *) closure);

    while ( formals != R_NilValue )
    {
	if (TAG(formals) ==  R_DotsSymbol) break;
	snprintf(tmp, 20, " %%%s", CHAR(PRINTNAME(TAG(formals))));
	tmp[20] = '\0';
	if (strlen(buf) + strlen(tmp) >= buflen)
	    error(_("argument list is too long in tcltk internal function 'callback_closure'"));
	strcat(buf, tmp);
	formals = CDR(formals);
    }
}

static void callback_lang(char *buf, SEXP call, SEXP env)
{
    sprintf(buf, "R_call_lang %p %p", (void *) call, (void *) env);

}

/* Setup to invoke callback from Tcl. Notice that something needs to
   ensure that the callback is protected from GC as long as anything
   is around to invoke it. This is handled in interpreted code by
   assigning into the environment of the window with which the
   callback is associated */

#define BUFFLEN 256
SEXP dotTclcallback(SEXP args)
{
    SEXP ans, callback = CADR(args), env;
    char buff[BUFFLEN];

    if (isFunction(callback))
        callback_closure(buff, BUFFLEN, callback);
    else if (isLanguage(callback)) {
        env = CADDR(args);
        callback_lang(buff, callback, env);
    }
    else
    	error(_("argument is not of correct type"));

#ifdef SUPPORT_MBCS
    {
	char *s;
	Tcl_DString s_ds;

	Tcl_DStringInit(&s_ds);
	s = Tcl_UtfToExternalDString(NULL, buff, -1, &s_ds);
	ans = mkString(s);
	Tcl_DStringFree(&s_ds);
    }
#else
    ans = mkString(buff);
#endif
    return ans;
}



#ifndef Win32
#define R_INTERFACE_PTRS 1
#include <Rinterface.h>  /* R_GUIType and more for console */
/* Add/delete Tcl/Tk event handler */

static void (* OldHandler)(void);
static int OldTimeout;
static int Tcl_loaded = 0;

static void TclHandler(void)
{
    while (Tcl_DoOneEvent(TCL_DONT_WAIT))
	;
    /* Tcl_ServiceAll is not enough here, for reasons that escape me */
    OldHandler();
}

static int Gtk_TclHandler(void)
{
    while (Tcl_DoOneEvent(TCL_DONT_WAIT))
	;
    return 1;
}

static void addTcl(void)
{
    if (Tcl_loaded)
	error(_("Tcl already loaded"));
    Tcl_loaded = 1;
    if (strcmp(R_GUIType, "GNOME") == 0) {
	/* This gets polled in the gnomeGUI console's event loop */
        R_timeout_handler = Gtk_TclHandler;
        R_timeout_val = 500;
    } else {
        OldHandler = R_PolledEvents;
	OldTimeout = R_wait_usec;
	R_PolledEvents = TclHandler;
	if ( R_wait_usec > 10000 || R_wait_usec == 0)
	    R_wait_usec = 10000;
    }
}

void delTcl(void)
{
    if (!Tcl_loaded)
	error(_("Tcl is not loaded"));
    if (strcmp(R_GUIType, "GNOME") == 0) {
        R_timeout_handler = NULL;
        R_timeout_val = 0;
    } else {
        if (R_PolledEvents != TclHandler)
	    error(_("Tcl is not last loaded handler"));
	R_PolledEvents = OldHandler;
	R_wait_usec = OldTimeout;
    }
    Tcl_loaded = 0;
}

/* ----- Event loop interface routines -------- */
static Tcl_Time timeout;

static void RTcl_setupProc(ClientData clientData, int flags)
{
    Tcl_SetMaxBlockTime(&timeout);
}
static void RTcl_eventProc(RTcl_Event *evPtr, int flags)
{
    fd_set *readMask = R_checkActivity(0 /*usec*/, 1 /*ignore_stdin*/);

    if (readMask==NULL)
	return;

    R_runHandlers(R_InputHandlers, readMask);
}
static void RTcl_checkProc(ClientData clientData, int flags)
{
    fd_set *readMask = R_checkActivity(0 /*usec*/, 1 /*ignore_stdin*/);
    RTcl_Event * evPtr;
    if (readMask==NULL)
	return;

    evPtr = (RTcl_Event*) Tcl_Alloc(sizeof(RTcl_Event));
    evPtr->proc = (Tcl_EventProc*) RTcl_eventProc;

    Tcl_QueueEvent((Tcl_Event*) evPtr, TCL_QUEUE_HEAD);
}

#endif

void tcltk_init(void)
{
    int code;

    /* Absence of the following line is said to be an error with
     * tcl 8.4 on all platforms, and is known to cause crashes under
     * Windows */

    /* Unfortunately, *presence* of the line appears to cause crashes
     * with tcl 8.0... */

#ifndef TCL80
    Tcl_FindExecutable(NULL);
#endif

    RTcl_interp = Tcl_CreateInterp();
    code = Tcl_Init(RTcl_interp); /* Undocumented... If omitted, you
				    get the windows but no event
				    handling. (Update: the
				    documentation is in the Tcl
				    sources, just not shipped
				    w/RedHat) */
    if (code != TCL_OK)
	error(Tcl_GetStringResult(RTcl_interp));

#if !defined(Win32) && !defined(HAVE_AQUA)
    if(getenv("DISPLAY")) 
#endif
    {
	code = Tk_Init(RTcl_interp);  /* Load Tk into interpreter */
	if (code != TCL_OK)
	    error(Tcl_GetStringResult(RTcl_interp));

	Tcl_StaticPackage(RTcl_interp, "Tk", Tk_Init, Tk_SafeInit);

	code = Tcl_Eval(RTcl_interp, "wm withdraw .");  /* Hide window */
	if (code != TCL_OK)
	    error(Tcl_GetStringResult(RTcl_interp));
    }
#if !defined(Win32) && !defined(HAVE_AQUA)
    else
	warning(_("no DISPLAY variable so Tk is not available"));
#endif

    Tcl_CreateCommand(RTcl_interp,
		      "R_eval",
		      R_eval,
		      (ClientData) NULL,
		      (Tcl_CmdDeleteProc *) NULL);

    Tcl_CreateCommand(RTcl_interp,
		      "R_call",
		      R_call,
		      (ClientData) NULL,
		      (Tcl_CmdDeleteProc *) NULL);

    Tcl_CreateCommand(RTcl_interp,
		      "R_call_lang",
		      R_call_lang,
		      (ClientData) NULL,
		      (Tcl_CmdDeleteProc *) NULL);

#ifdef Win32
    Tcl_SetServiceMode(TCL_SERVICE_ALL);
#else
    addTcl(); /* notice: this sets R_wait_usec.... */
    timeout.sec = 0;
    timeout.usec = R_wait_usec;
    Tcl_CreateEventSource(RTcl_setupProc, RTcl_checkProc, 0);
#endif

/*** We may want to revive this at some point ***/

#if 0
  code = Tcl_EvalFile(RTcl_interp, "init.tcl");
  if (code != TCL_OK)
    error("%s\n", Tcl_GetStringResult(RTcl_interp));
#endif

}

SEXP RTcl_ServiceMode(SEXP args)
{
    SEXP ans;
    int value;
    
    if (!isLogical(CADR(args)) || length(CADR(args)) > 1)
    	error(_("invalid argument"));
    
    if (length(CADR(args))) value = Tcl_SetServiceMode(LOGICAL(CADR(args))[0] ? 
    						TCL_SERVICE_ALL : TCL_SERVICE_NONE);
    else {
    	value = Tcl_SetServiceMode(TCL_SERVICE_NONE);
    	if (value != TCL_SERVICE_NONE) Tcl_SetServiceMode(value); /* Tcl_GetServiceMode was not found */
    }
    
    ans = allocVector(LGLSXP, 1);
    LOGICAL(ans)[0] = value == TCL_SERVICE_ALL;
    return ans;
}
    
#ifndef Win32
#ifndef TCL80
/* ----- Tcl/Tk console routines ----- */

/* From former src/unix/devUI.h
extern int  (*ptr_R_ReadConsole)(char *, unsigned char *, int, int);
extern void (*ptr_R_WriteConsole)(char *, int);
extern void (*ptr_R_ResetConsole)();
extern void (*ptr_R_FlushConsole)();
extern void (*ptr_R_ClearerrConsole)();
extern FILE * R_Consolefile;
extern FILE * R_Outputfile; */

/* Fill a text buffer with user typed console input. */

static int
RTcl_ReadConsole (char *prompt, unsigned char *buf, int len,
		  int addtohistory)
{
    Tcl_Obj *cmd[3];
    int i, code;

    cmd[0] = Tcl_NewStringObj("Rc_read", -1);
    cmd[1] = Tcl_NewStringObj(prompt, -1);
    cmd[2] = Tcl_NewIntObj(addtohistory);

    for (i = 0 ; i < 3 ; i++)
	Tcl_IncrRefCount(cmd[i]);

    code = Tcl_EvalObjv(RTcl_interp, 3, cmd, 0);
    if (code != TCL_OK)
	return 0;
    else
#ifdef SUPPORT_MBCS
    {
	    char *buf_utf8;
	    Tcl_DString buf_utf8_ds;
	    Tcl_DStringInit(&buf_utf8_ds);
	    buf_utf8 =
		    Tcl_UtfToExternalDString(NULL,
		    			     Tcl_GetStringResult(RTcl_interp),
					     len,
					     &buf_utf8_ds);
            strncpy((char *)buf, buf_utf8, len);
	    Tcl_DStringFree(&buf_utf8_ds);
    }
#else /* SUPPORT_MBCS */
	strncpy((char *)buf, (char *) Tcl_GetStringResult(RTcl_interp), len);
#endif /* SUPPORT_MBCS */

    /* At some point we need to figure out what to do if the result is
     * longer than "len"... For now, just truncate. */

    for (i = 0 ; i < 3 ; i++)
	Tcl_DecrRefCount(cmd[i]);

    return 1;
}

/* Write a text buffer to the console. */
/* All system output is filtered through this routine. */
static void
RTcl_WriteConsole (char *buf, int len)
{
    Tcl_Obj *cmd[2];
#ifdef SUPPORT_MBCS
    char *buf_utf8;
    Tcl_DString  buf_utf8_ds;

    Tcl_DStringInit(&buf_utf8_ds);
    buf_utf8 = Tcl_ExternalToUtfDString(NULL, buf, -1, &buf_utf8_ds);
#endif /* SUPPORT_MBCS */

    /* Construct command */
    cmd[0] = Tcl_NewStringObj("Rc_write", -1);
#ifdef SUPPORT_MBCS
    cmd[1] = Tcl_NewStringObj(buf_utf8, -1);
#else /* SUPPORT_MBCS */
    cmd[1] = Tcl_NewStringObj(buf, len);
#endif /* SUPPORT_MBCS */

    Tcl_IncrRefCount(cmd[0]);
    Tcl_IncrRefCount(cmd[1]);

    Tcl_EvalObjv(RTcl_interp, 2, cmd, 0);

    Tcl_DecrRefCount(cmd[0]);
    Tcl_DecrRefCount(cmd[1]);
#ifdef SUPPORT_MBCS
    Tcl_DStringFree(&buf_utf8_ds);
#endif /* SUPPORT_MBCS */
}

/* Indicate that input is coming from the console */
static void
RTcl_ResetConsole ()
{
}

/* Stdio support to ensure the console file buffer is flushed */
static void
RTcl_FlushConsole ()
{
}


/* Reset stdin if the user types EOF on the console. */
static void
RTcl_ClearerrConsole ()
{
}

void RTcl_ActivateConsole ()
{
    ptr_R_ReadConsole = RTcl_ReadConsole;
    ptr_R_WriteConsole = RTcl_WriteConsole;
    ptr_R_ResetConsole = RTcl_ResetConsole;
    ptr_R_FlushConsole = RTcl_FlushConsole;
    ptr_R_ClearerrConsole = RTcl_ClearerrConsole;
    R_Consolefile = NULL;
    R_Outputfile = NULL;
}
#endif
#endif
