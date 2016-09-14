/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000--2013  The R Core Team
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
# include <config.h>
#endif
#define NO_NLS
#include <Defn.h>

#include "tcltk.h" /* declarations of our `public' interface */
#include <stdlib.h>

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("tcltk", String)
#else
#define _(String) (String)
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
		  const char *argv[])
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
	R_Busy(1);
	int n = length(expr);
	for(i = 0 ; i < n ; i++)
	    ans = eval(VECTOR_ELT(expr, i), R_GlobalEnv);
	PROTECT(ans);
	R_Busy(0);
    }

    /* If return value is of class tclObj, use as Tcl result */
    if (inherits(ans, "tclObj"))
	    Tcl_SetObjResult(interp, (Tcl_Obj*) R_ExternalPtrAddr(ans));

    UNPROTECT(3);
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
		  const char *argv[])
{
    int i;
    SEXP expr, alist, ans;
    void *fun;

    SEXP s_try = install("try");

    alist = R_NilValue;
    for (i = argc - 1 ; i > 1 ; i--){
	PROTECT(alist);
	alist = LCONS(mkString(argv[i]), alist);
	UNPROTECT(1);
    }

    sscanf(argv[1], "%p", &fun);

    expr = LCONS( (SEXP)fun, alist);
    PROTECT(expr = LCONS(s_try, LCONS(expr, R_NilValue)));

    R_Busy(1);
    PROTECT(ans = eval(expr, R_GlobalEnv));
    R_Busy(0);
	
    /* If return value is of class tclObj, use as Tcl result */
    if (inherits(ans, "tclObj"))
	Tcl_SetObjResult(interp, (Tcl_Obj*) R_ExternalPtrAddr(ans));

    UNPROTECT(2);
    return TCL_OK;
}

static int R_call_lang(ClientData clientData,
		       Tcl_Interp *interp,
		       int argc,
		       const char *argv[])
{
    void *expr, *env; 
    SEXP ans;

    sscanf(argv[1], "%p", &expr);
    sscanf(argv[2], "%p", &env);

    SEXP s_try = install("try");
    expr = LCONS(s_try, LCONS(expr, R_NilValue));
    PROTECT((SEXP)expr);

    R_Busy(1);
    PROTECT(ans = eval((SEXP)expr, (SEXP)env));
    R_Busy(0);

    /* If return value is of class tclObj, use as Tcl result */
    if (inherits(ans, "tclObj"))
	Tcl_SetObjResult(interp, (Tcl_Obj*) R_ExternalPtrAddr(ans));

    UNPROTECT(2);
    return TCL_OK;
}


static Tcl_Obj * tk_eval(const char *cmd)
{
    char *cmd_utf8;
    Tcl_DString  cmd_utf8_ds;

    Tcl_DStringInit(&cmd_utf8_ds);
    cmd_utf8 = Tcl_ExternalToUtfDString(NULL, cmd, -1, &cmd_utf8_ds);
    if (Tcl_Eval(RTcl_interp, cmd_utf8) == TCL_ERROR)
    {
	char p[512];
	if (strlen(Tcl_GetStringResult(RTcl_interp)) > 500)
	    strcpy(p, _("tcl error.\n"));
	else {
	    char *res;
	    Tcl_DString  res_ds;

	    Tcl_DStringInit(&res_ds);
	    res = Tcl_UtfToExternalDString(NULL,
					   Tcl_GetStringResult(RTcl_interp),
					   -1, &res_ds);
	    snprintf(p, sizeof(p), "[tcl] %s.\n", res);
	    Tcl_DStringFree(&res_ds);
	}
	error(p);
    }
    Tcl_DStringFree(&cmd_utf8_ds);
    return Tcl_GetObjResult(RTcl_interp);
}

/* FIXME get rid of fixed size buffers in do_Tclcallback et al. (low
   priority, since there is no meaningful reason to exceed the current
   buffer length) */

SEXP dotTcl(SEXP args)
{
    SEXP ans;
    const char *cmd;
    Tcl_Obj *val;
    const void *vmax = vmaxget();
    if(!isValidString(CADR(args)))
	error(_("invalid argument"));
    cmd = translateChar(STRING_ELT(CADR(args), 0));
    val = tk_eval(cmd);
    ans = makeRTclObject(val);
    vmaxset(vmax);
    return ans;
}

SEXP dotTclObjv(SEXP args)
{
    SEXP t,
	avec = CADR(args),
	nm = getAttrib(avec, R_NamesSymbol);
    int objc, i, result;
    Tcl_Obj **objv;
    const void *vmax = vmaxget();

    for (objc = 0, i = 0; i < length(avec); i++){
	if (!isNull(VECTOR_ELT(avec, i)))
	    objc++;
	if (!isNull(nm) && strlen(translateChar(STRING_ELT(nm, i))))
	    objc++;
    }

    objv = (Tcl_Obj **) R_alloc(objc, sizeof(Tcl_Obj *));

    for (objc = i = 0; i < length(avec); i++){
	const char *s;
	char *tmp;
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
	else {
	    char *res;
	    Tcl_DString  res_ds;
	    Tcl_DStringInit(&res_ds);
	    res = Tcl_UtfToExternalDString(NULL,
					   Tcl_GetStringResult(RTcl_interp),
					   -1, &res_ds);
	    snprintf(p, sizeof(p), "[tcl] %s.\n", res);
	    Tcl_DStringFree(&res_ds);
	}
	error(p);
    }

    SEXP res = makeRTclObject(Tcl_GetObjResult(RTcl_interp));
    vmaxset(vmax);
    return res;
}


SEXP RTcl_ObjFromVar(SEXP args)
{
    Tcl_Obj *tclobj;
    const void *vmax = vmaxget();

    tclobj = Tcl_GetVar2Ex(RTcl_interp,
                           translateChar(STRING_ELT(CADR(args), 0)),
                           NULL,
                           0);
    SEXP res = makeRTclObject(tclobj);
    vmaxset(vmax);
    return res;
}

SEXP RTcl_AssignObjToVar(SEXP args)
{
    const void *vmax = vmaxget();
    Tcl_SetVar2Ex(RTcl_interp,
		  translateChar(STRING_ELT(CADR(args), 0)),
		  NULL,
		  (Tcl_Obj *) R_ExternalPtrAddr(CADDR(args)),
		  0);
    vmaxset(vmax);
    return R_NilValue;
}


SEXP RTcl_StringFromObj(SEXP args)
{
    char *str;
    SEXP so;
    char *s;
    Tcl_DString s_ds;
    Tcl_Obj *obj;

    obj = (Tcl_Obj *) R_ExternalPtrAddr(CADR(args));
    if (!obj) error(_("invalid tclObj -- perhaps saved from another session?"));
    Tcl_DStringInit(&s_ds);
    str = Tcl_GetStringFromObj(obj, NULL);
    /* FIXME: could use UTF-8 here */
    s = Tcl_UtfToExternalDString(NULL, str, -1, &s_ds);
    so = mkString(s);
    Tcl_DStringFree(&s_ds);
    return(so);
}

SEXP RTcl_ObjAsCharVector(SEXP args)
{
    int count;
    Tcl_Obj **elem, *obj;
    int ret, i;
    SEXP ans;

    obj = (Tcl_Obj *) R_ExternalPtrAddr(CADR(args));
    if (!obj) error(_("invalid tclObj -- perhaps saved from another session?"));
    ret = Tcl_ListObjGetElements(RTcl_interp, obj, &count, &elem);
    if (ret != TCL_OK)
	return RTcl_StringFromObj(args);

    PROTECT(ans = allocVector(STRSXP, count));
    for (i = 0 ; i < count ; i++) {
	char *s;
	Tcl_DString s_ds;
	Tcl_DStringInit(&s_ds);
	/* FIXME: could use UTF-8 here */
	s = Tcl_UtfToExternalDString(NULL,
				     (Tcl_GetStringFromObj(elem[i], NULL)),
				     -1, &s_ds);
	SET_STRING_ELT(ans, i, mkChar(s));
	Tcl_DStringFree(&s_ds);
    }
    UNPROTECT(1);
    return ans;
}

SEXP RTcl_ObjFromCharVector(SEXP args)
{
    char *s;
    Tcl_DString s_ds;
    int count;
    Tcl_Obj *tclobj, *elem;
    int i;
    SEXP val, drop;
    Tcl_Encoding encoding;
    const void *vmax = vmaxget();

    val = CADR(args);
    drop = CADDR(args);

    tclobj = Tcl_NewObj();

    count = length(val);
    encoding = Tcl_GetEncoding(RTcl_interp, "utf-8");
    if (count == 1 && LOGICAL(drop)[0]) {
	Tcl_DStringInit(&s_ds);
	s = Tcl_ExternalToUtfDString(encoding,
				     translateCharUTF8(STRING_ELT(val, 0)), 
				     -1, &s_ds);
	Tcl_SetStringObj(tclobj, s, -1);
	Tcl_DStringFree(&s_ds);
    } else
	for ( i = 0 ; i < count ; i++) {
	    elem = Tcl_NewObj();
	    Tcl_DStringInit(&s_ds);
	    s = Tcl_ExternalToUtfDString(encoding, 
					 translateCharUTF8(STRING_ELT(val, i)),
					 -1, &s_ds);
	    Tcl_SetStringObj(elem, s, -1);
	    Tcl_DStringFree(&s_ds);
	    Tcl_ListObjAppendElement(RTcl_interp, tclobj, elem);
	}

    Tcl_FreeEncoding(encoding);
    SEXP res = makeRTclObject(tclobj);
    vmaxset(vmax);
    return res;
}

SEXP RTcl_ObjAsDoubleVector(SEXP args)
{
    int count;
    Tcl_Obj **elem, *obj;
    int ret, i;
    double x;
    SEXP ans;

    obj = (Tcl_Obj *) R_ExternalPtrAddr(CADR(args));
    if (!obj) error(_("invalid tclObj -- perhaps saved from another session?"));

    /* First try for single value */
    ret = Tcl_GetDoubleFromObj(RTcl_interp, obj, &x);
    if (ret == TCL_OK) return ScalarReal(x);

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
    if (!obj) error(_("invalid tclObj -- perhaps saved from another session?"));

    /* First try for single value */
    ret = Tcl_GetIntFromObj(RTcl_interp, obj, &x);
    if (ret == TCL_OK) return ScalarInteger(x);

    /* Then try as list */
    ret = Tcl_ListObjGetElements(RTcl_interp, obj, &count, &elem);
    if (ret != TCL_OK) /* didn't work, return NULL */
	return R_NilValue;

    ans = allocVector(INTSXP, count);
    for (i = 0 ; i < count ; i++){
	ret = Tcl_GetIntFromObj(RTcl_interp, elem[i], &x);
	if (ret != TCL_OK) x = NA_INTEGER;
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

SEXP RTcl_ObjAsRawVector(SEXP args)
{
    int nb, count, i, j;
    Tcl_Obj **elem, *obj;
    unsigned char *ret;
    SEXP ans, el;

    obj = (Tcl_Obj *) R_ExternalPtrAddr(CADR(args));
    if (!obj) error(_("invalid tclObj -- perhaps saved from another session?"));
    ret = Tcl_GetByteArrayFromObj(obj, &nb);
    if (ret) {
	ans = allocVector(RAWSXP, nb);
	for (j = 0 ; j < nb ; j++) RAW(ans)[j] = ret[j];
	return ans;
    }

    /* Then try as list */
    if (Tcl_ListObjGetElements(RTcl_interp, obj, &count, &elem)
	!= TCL_OK) return R_NilValue;

    PROTECT(ans = allocVector(VECSXP, count));
    for (i = 0 ; i < count ; i++) {
	el = allocVector(RAWSXP, nb);
	SET_VECTOR_ELT(ans, i, el);
	ret = Tcl_GetByteArrayFromObj(elem[i], &nb);
	for (j = 0 ; j < nb ; j++) RAW(el)[j] = ret[j];
    }
    UNPROTECT(1);
    return ans;
}

SEXP RTcl_ObjFromRawVector(SEXP args)
{
    int count;
    Tcl_Obj *tclobj; 
    SEXP val; 

    val = CADR(args);

    count = length(val);
    tclobj = Tcl_NewByteArrayObj(RAW(val), count);

    return makeRTclObject(tclobj);
}


SEXP RTcl_GetArrayElem(SEXP args)
{
    SEXP x, i;
    const char *xstr, *istr;
    Tcl_Obj *tclobj;
    const void *vmax = vmaxget();

    x = CADR(args);
    i = CADDR(args);

    xstr = translateChar(STRING_ELT(x, 0));
    istr = translateChar(STRING_ELT(i, 0));
    tclobj = Tcl_GetVar2Ex(RTcl_interp, xstr, istr, 0);
    vmaxset(vmax);

    if (tclobj == NULL)
	return R_NilValue;
    else
	return makeRTclObject(tclobj);
}

SEXP RTcl_SetArrayElem(SEXP args)
{
    SEXP x, i;
    const char *xstr, *istr;
    Tcl_Obj *value;
    const void *vmax = vmaxget();

    x = CADR(args);
    i = CADDR(args);
    value = (Tcl_Obj *) R_ExternalPtrAddr(CADDDR(args));

    xstr = translateChar(STRING_ELT(x, 0));
    istr = translateChar(STRING_ELT(i, 0));
    Tcl_SetVar2Ex(RTcl_interp, xstr, istr, value, 0);

    vmaxset(vmax);
    return R_NilValue;
}

SEXP RTcl_RemoveArrayElem(SEXP args)
{
    SEXP x, i;
    const char *xstr, *istr;
    const void *vmax = vmaxget();

    x = CADR(args);
    i = CADDR(args);

    xstr = translateChar(STRING_ELT(x, 0));
    istr = translateChar(STRING_ELT(i, 0));
    Tcl_UnsetVar2(RTcl_interp, xstr, istr, 0);
    vmaxset(vmax);

    return R_NilValue;
}

static void callback_closure(char * buf, int buflen, SEXP closure)
{
    static char tmp[21];
    SEXP formals;

    formals = FORMALS(closure);

    snprintf(buf, buflen, "R_call %p", (void *) closure);

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

static void callback_lang(char *buf, int buflen, SEXP call, SEXP env)
{
    snprintf(buf, buflen, "R_call_lang %p %p", (void *) call, (void *) env);

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
    char *s;
    Tcl_DString s_ds;

    if (isFunction(callback))
        callback_closure(buff, BUFFLEN, callback);
    else if (isLanguage(callback)) {
        env = CADDR(args);
        callback_lang(buff, BUFFLEN, callback, env);
    }
    else
    	error(_("argument is not of correct type"));

    Tcl_DStringInit(&s_ds);
    s = Tcl_UtfToExternalDString(NULL, buff, -1, &s_ds);
    ans = mkString(s);
    Tcl_DStringFree(&s_ds);
    return ans;
}


#include <tk.h>


void tcltk_init(int *TkUp)
{
    int code;

    *TkUp = 0;

    /* Absence of the following line is said to be an error with
     * tcl >= 8.4 on all platforms, and is known to cause crashes under
     * Windows */

    Tcl_FindExecutable(NULL);

    RTcl_interp = Tcl_CreateInterp();
    code = Tcl_Init(RTcl_interp);
    if (code != TCL_OK) error(Tcl_GetStringResult(RTcl_interp));

/* HAVE_AQUA is not really right here.
   On macOS we might be using Aqua Tcl/Tk or X11 Tcl/Tk, and that
   is in principle independent of whether we want quartz() built.
*/
#if !defined(Win32) && !defined(HAVE_AQUA)
    char *p= getenv("DISPLAY");
    if(p && p[0])  /* exclude DISPLAY = "" */
#endif
    {
	code = Tk_Init(RTcl_interp);  /* Load Tk into interpreter */
	if (code != TCL_OK) {
	    warning(Tcl_GetStringResult(RTcl_interp));
	} else {
	    Tcl_StaticPackage(RTcl_interp, "Tk", Tk_Init, Tk_SafeInit);

	    code = Tcl_Eval(RTcl_interp, "wm withdraw .");  /* Hide window */
	    if (code != TCL_OK) error(Tcl_GetStringResult(RTcl_interp));
	    *TkUp = 1;
	}
    }
#if !defined(Win32) && !defined(HAVE_AQUA)
    else
	warningcall(R_NilValue, _("no DISPLAY variable so Tk is not available"));
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

#ifndef Win32
    Tcl_unix_setup();
#endif
    Tcl_SetServiceMode(TCL_SERVICE_ALL);

/*** We may want to revive this at some point ***/

#if 0
  code = Tcl_EvalFile(RTcl_interp, "init.tcl");
  if (code != TCL_OK)
    error("%s\n", Tcl_GetStringResult(RTcl_interp));
#endif

}

SEXP RTcl_ServiceMode(SEXP args)
{
    int value;
    
    if (!isLogical(CADR(args)) || length(CADR(args)) > 1)
    	error(_("invalid argument"));
    
    if (length(CADR(args))) 
	value = Tcl_SetServiceMode(LOGICAL(CADR(args))[0] ? 
				   TCL_SERVICE_ALL : TCL_SERVICE_NONE);
    else
    	value = Tcl_GetServiceMode();
    
    return ScalarLogical(value == TCL_SERVICE_ALL);
}
    
