/*******************************************************************************
 *  RProxy: Connector implementation between application and R language
 *  Copyright (C) 1999--2005 Thomas Baier
 *
 *  R_Proxy_init based on rtest.c,  Copyright (C) 1998--2000
 *                                  R Development Core Team
 *
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public
 *  License as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public
 *  License along with this library; if not, write to the Free
 *  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 *  MA 02111-1307, USA
 *
 ******************************************************************************/

#define NONAMELESSUNION
#include <windows.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
/*#include "globalvar.h"
#undef CharacterMode
#undef R_Interactive*/
#include <config.h>
#include <Rversion.h>
#include <Startup.h>
#include "bdx_SEXP.h"
#include "SC_proxy.h"
#include "rproxy.h"
#include "rproxy_impl.h"
#include <IOStuff.h>
#include <Parse.h>
#include <Graphics.h>

struct _R_Proxy_init_parameters
{
  int vsize;
  int vsize_valid;
  int nsize;
  int nsize_valid;
};

/* 01-12-07 | baier | no more extern for exported variables (crashes!) */
/*#define R_GlobalEnv (*__imp_R_GlobalEnv)
#define R_Visible (*__imp_R_Visible)
#define R_EvalDepth (*__imp_R_EvalDepth)
#define R_DimSymbol (*__imp_R_DimSymbol)*/

/*
extern SEXP R_GlobalEnv;
extern int R_Visible;
extern int R_EvalDepth;
extern SEXP R_DimSymbol;
*/

/* calls into the R DLL */
extern char *getDLLVersion();
extern void R_DefParams(Rstart);
extern void R_SetParams(Rstart);
extern void setup_term_ui(void);
extern char *getRHOME();
extern void end_Rmainloop(), R_ReplDLLinit();
extern void askok(char *);

int R_Proxy_Graphics_Driver (NewDevDesc* pDD,
			     char* pDisplay,
			     double pWidth,
			     double pHeight,
			     double pPointSize);

extern SC_CharacterDevice* __output_device;

/* trace to DebugView */

int R_Proxy_printf(char const* pFormat,...)
{
  static char __tracebuf[2048];

  va_list lArgs;
  va_start(lArgs,pFormat);
  vsprintf(__tracebuf,pFormat,lArgs);
  OutputDebugString(__tracebuf);
  return 0;
}

static int s_EvalInProgress = 0;

void R_Proxy_askok (char* pMsg)
{
  askok(pMsg);
  return;
}

int R_Proxy_askyesnocancel (char* pMsg)
{
  return 1;
}

int R_Proxy_ReadConsole(char *prompt, char *buf, int len, int addtohistory)
{
  return 0;
}

void R_Proxy_WriteConsole(char *buf, int len)
{
  if (__output_device)
    {
      __output_device->vtbl->write_string (__output_device,buf);
    }
}

void R_Proxy_CallBack()
{
    /* called during i/o, eval, graphics in ProcessEvents */
}

void R_Proxy_Busy(int which)
{
    /* set a busy cursor ... in which = 1, unset if which = 0 */
}

/* 00-02-18 | baier | parse parameter string and fill parameter structure */
int R_Proxy_parse_parameters (char const* pParameterString,
			      struct _R_Proxy_init_parameters* pParameterStruct)
{
  /*
   * parameter string is of the form name1=value1;name2=value2;...
   *
   * currently recognized parameter names (case-sensitive):
   *
   *   NSIZE ... number of cons cells, (unsigned int) parameter
   *   VSIZE ... size of vector heap, (unsigned int) parameter
   */
  int lDone = 0;
#if 0
  char const* lParameterStart = pParameterString;
  int lIndexOfSemicolon = 0;
  char* lTmpBuffer = NULL;
  char* lPosOfSemicolon = NULL;
#endif

  while (!lDone)
    {
#if 0
      /* NSIZE? */
      if (strncmp (lParameterStart,"NSIZE=",6) == 0)
	{
	  lParameterStart += 6;

	  lPosOfSemicolon = strchr (lParameterStart,';');
	  lIndexOfSemicolon = lPosOfSemicolon - lParameterStart;

	  if (lPosOfSemicolon)
	    {
	      lTmpBuffer = malloc (lIndexOfSemicolon + 1); /* to catch NSIZE=; */
	      strncpy (lTmpBuffer,lParameterStart,lIndexOfSemicolon);
	      *(lTmpBuffer + lIndexOfSemicolon) = 0x0;
	      pParameterStruct->nsize_valid = 1;
	      pParameterStruct->nsize = atoi (lTmpBuffer);
	      free (lTmpBuffer);
	      lParameterStart += lIndexOfSemicolon + 1;
	    }
	  else
	    {
	      pParameterStruct->nsize_valid = 1;
	      pParameterStruct->nsize = atoi (lParameterStart);
	      lDone = 1;
	    }
	}
      else if (strncmp (lParameterStart,"VSIZE=",6) == 0)
	{
	  lParameterStart += 6;

	  lPosOfSemicolon = strchr (lParameterStart,';');
	  lIndexOfSemicolon = lPosOfSemicolon - lParameterStart;

	  if (lPosOfSemicolon)
	    {
	      lTmpBuffer = malloc (lIndexOfSemicolon + 1); /* to catch VSIZE=; */
	      strncpy (lTmpBuffer,lParameterStart,lIndexOfSemicolon);
	      *(lTmpBuffer + lIndexOfSemicolon) = 0x0;
	      pParameterStruct->vsize_valid = 1;
	      pParameterStruct->vsize = atoi (lTmpBuffer);
	      free (lTmpBuffer);
	      lParameterStart += lIndexOfSemicolon + 1;
	    }
	  else
	    {
	      pParameterStruct->vsize_valid = 1;
	      pParameterStruct->vsize = atoi (lParameterStart);
	      lDone = 1;
	    }
	}
      else
#endif
	{
	  lDone = 1;
	}
    }

  return 0;
}

#include "../shext.h" /* for ShellGetPersonalDirectory */

/* 00-02-18 | baier | R_Proxy_init() now takes parameter string, parse it */
/* 03-06-01 | baier | now we add %R_HOME%\bin to %PATH% */
int R_Proxy_init (char const* pParameterString)
{
  structRstart rp;
  Rstart Rp = &rp;
  char Rversion[25];
  static char RUser[MAX_PATH], RHome[MAX_PATH];
  char *p, *q;

  sprintf(Rversion, "%s.%s", R_MAJOR, R_MINOR);
  if(strcmp(getDLLVersion(), Rversion) != 0) {
    fprintf(stderr, "Error: R.DLL version does not match\n");
    return SC_PROXY_ERR_UNKNOWN;
  }

  R_DefParams(Rp);

  /* first, try process-local environment space (CRT) */
  if (getenv("R_HOME")) {
      strcpy(RHome, getenv("R_HOME"));
  } else {
    /* get variable from process-local environment space (Windows API) */
      if (GetEnvironmentVariable ("R_HOME", RHome, sizeof (RHome)) == 0) {
	/* not found, fall back to getRHOME() */
	strcpy(RHome, getRHOME());
      }
    }

  /* now we add %R_HOME%\bin to %PATH% (for dynamically loaded modules there) */
  {
    char buf[2048];
    sprintf(buf,"PATH=%s\\bin;%s",RHome,getenv("PATH"));
    putenv(buf);
  }

  Rp->rhome = RHome;

  /*
   * try R_USER then HOME then Windows homes then working directory
   */

  if ((p = getenv("R_USER"))) {
    if(strlen(p) >= MAX_PATH) R_Suicide("Invalid R_USER");
    strcpy(RUser, p);
  } else if ((p = getenv("HOME"))) {
    if(strlen(p) >= MAX_PATH) R_Suicide("Invalid HOME");
    strcpy(RUser, p);
  } else if (ShellGetPersonalDirectory(RUser)) {
    /* nothing to do */;
  } else if ((p = getenv("HOMEDRIVE")) && (q = getenv("HOMEPATH"))) {
    if(strlen(p) >= MAX_PATH) R_Suicide("Invalid HOMEDRIVE");
    strcpy(RUser, p);
    if(strlen(RUser) + strlen(q) >= MAX_PATH)
      R_Suicide("Invalid HOMEDRIVE+HOMEPATH");
    strcat(RUser, q);
  } else {
    GetCurrentDirectory(MAX_PATH, RUser);
  }
  
  p = RUser + (strlen(RUser) - 1);

  if (*p == '/' || *p == '\\') *p = '\0';
  Rp->home = RUser;
  Rp->CharacterMode = LinkDLL;
  Rp->ReadConsole = R_Proxy_ReadConsole;
  Rp->WriteConsole = R_Proxy_WriteConsole;
  Rp->CallBack = R_Proxy_CallBack;
  Rp->ShowMessage = R_Proxy_askok;

  Rp->YesNoCancel = R_Proxy_askyesnocancel;
  Rp->Busy = R_Proxy_Busy;
  Rp->R_Quiet = 1;
#if 1
  /* run as "interactive", so server won't be killed after an error */
  Rp->R_Slave = Rp->R_Verbose = 0;
  Rp->R_Interactive = 1;
#else
  Rp->R_Slave = Rp->R_Interactive = Rp->R_Verbose = 0;
#endif
  Rp->RestoreAction = 0; /* no restore */
  Rp->SaveAction = 2;    /* no save */

/*  Rp->nsize = 300000;
    Rp->vsize = 6e6;*/
  R_SetParams(Rp); /* so R_ShowMessage is set */
  R_SizeFromEnv(Rp);
  R_set_command_line_arguments(0, NULL);

  /* parse parameters */
#if 0
  {
    struct _R_Proxy_init_parameters lParameterStruct =
    {
      0,0,0,0
    };

    R_Proxy_parse_parameters (pParameterString,&lParameterStruct);

    if (lParameterStruct.nsize_valid)
      {
	Rp->nsize = lParameterStruct.nsize;
      }
    if (lParameterStruct.vsize_valid)
      {
	Rp->vsize = lParameterStruct.vsize;
      }
  }
#endif

  R_SetParams(Rp);

  setup_term_ui();
  setup_Rmainloop();
  R_ReplDLLinit();

  return SC_PROXY_OK;
}

/* 01-06-05 | baier | SETJMP and fatal error handling around eval() */
/* 04-08-01 | baier | ref-counting in case of error */
/* 04-10-11 | baier | restore original ref-counting */
/* 05-05-15 | baier | rework SETJMP code (store/restore jmp_buf) */
int R_Proxy_evaluate (char const* pCmd,BDX_Data** pData)
{
  SEXP rho = R_GlobalEnv;
  IoBuffer lBuffer;
  SEXP lSexp;
  int lRc;
  ParseStatus lStatus;
  SEXP lResult;

  /* for SETJMP/LONGJMP */
  s_EvalInProgress = 0;

  R_IoBufferInit (&lBuffer);
  R_IoBufferPuts ((char*) pCmd,&lBuffer);
  R_IoBufferPuts ("\n",&lBuffer);

  /* don't generate code, just a try */
  R_IoBufferReadReset (&lBuffer);
  lSexp = R_Parse1Buffer (&lBuffer,0,&lStatus);

  switch (lStatus)
    {
    case PARSE_NULL:
      /* we forget the IoBuffer "lBuffer", so don't do anything here */
      lRc = SC_PROXY_ERR_PARSE_INVALID;
      break;
    case PARSE_OK:
      /* now generate code */
      R_IoBufferReadReset (&lBuffer);
      lSexp = R_Parse1Buffer (&lBuffer,1,&lStatus);
      R_Visible = 0;
      R_EvalDepth = 0;
      PROTECT(lSexp);
      {
	JMP_BUF lJmpBuf;
	memcpy(lJmpBuf,R_Toplevel.cjmpbuf,sizeof(lJmpBuf));
	SETJMP (R_Toplevel.cjmpbuf);
	R_GlobalContext = R_ToplevelContext = &R_Toplevel;

	if (!s_EvalInProgress)
	  {
	    s_EvalInProgress = 1;
	    lResult = eval (lSexp,rho);
	    memcpy(R_Toplevel.cjmpbuf,lJmpBuf,sizeof(lJmpBuf));
	    s_EvalInProgress = 0;
	  }
	else
	  {
	    memcpy(R_Toplevel.cjmpbuf,lJmpBuf,sizeof(lJmpBuf));
	    return SC_PROXY_ERR_EVALUATE_STOP;
	  }
      }
      lRc = SEXP2BDX(lResult,pData);
      /* no last value */
      UNPROTECT(1);
      break;
    case PARSE_ERROR:
      lRc = SC_PROXY_ERR_PARSE_INVALID;
      break;
    case PARSE_INCOMPLETE:
      lRc = SC_PROXY_ERR_PARSE_INCOMPLETE;
      break;
    case PARSE_EOF:
      lRc = SC_PROXY_ERR_PARSE_INVALID;
      break;
    default:
      /* never reached */
      lRc = SC_PROXY_ERR_UNKNOWN;
      break;
    }

  return lRc;
}

/* 01-06-05 | baier | SETJMP and fatal error handling around eval() */
/* 04-08-01 | baier | ref-counting in case of error */
/* 04-10-11 | baier | restore original ref-counting */
/* 05-05-15 | baier | rework SETJMP code (store/restore jmp_buf) */
int R_Proxy_evaluate_noreturn (char const* pCmd)
{
  SEXP rho = R_GlobalEnv;
  IoBuffer lBuffer;
  SEXP lSexp;
  int lRc;
  ParseStatus lStatus;

  /* for SETJMP/LONGJMP */
  s_EvalInProgress = 0;

  R_IoBufferInit (&lBuffer);
  R_IoBufferPuts ((char*) pCmd,&lBuffer);
  R_IoBufferPuts ("\n",&lBuffer);

  /* don't generate code, just a try */
  R_IoBufferReadReset (&lBuffer);
  lSexp = R_Parse1Buffer (&lBuffer,0,&lStatus);

  switch (lStatus)
    {
    case PARSE_NULL:
      /* we forget the IoBuffer "lBuffer", so don't do anything here */
      lRc = SC_PROXY_ERR_PARSE_INVALID;
      break;
    case PARSE_OK:
      /* now generate code */
      R_IoBufferReadReset (&lBuffer);
      lSexp = R_Parse1Buffer (&lBuffer,1,&lStatus);
      R_Visible = 0;
      R_EvalDepth = 0;
      PROTECT(lSexp);
      /* at the moment, discard the result of the eval */
      {
	JMP_BUF lJmpBuf;
	memcpy(lJmpBuf,R_Toplevel.cjmpbuf,sizeof(lJmpBuf));
	SETJMP (R_Toplevel.cjmpbuf);
	R_GlobalContext = R_ToplevelContext = &R_Toplevel;

	if (!s_EvalInProgress)
	  {
	    s_EvalInProgress = 1;
	    eval (lSexp,rho);
	    memcpy(R_Toplevel.cjmpbuf,lJmpBuf,sizeof(lJmpBuf));
	    s_EvalInProgress = 0;
	  }
	else
	  {
	    memcpy(R_Toplevel.cjmpbuf,lJmpBuf,sizeof(lJmpBuf));
	    return SC_PROXY_ERR_EVALUATE_STOP;
	  }
      }
      /* no last value */
      UNPROTECT(1);
      lRc = SC_PROXY_OK;
      break;
    case PARSE_ERROR:
      lRc = SC_PROXY_ERR_PARSE_INVALID;
      break;
    case PARSE_INCOMPLETE:
      lRc = SC_PROXY_ERR_PARSE_INCOMPLETE;
      break;
    case PARSE_EOF:
      lRc = SC_PROXY_ERR_PARSE_INVALID;
      break;
    default:
      /* never reached */
      lRc = SC_PROXY_ERR_UNKNOWN;
      break;
    }

  return lRc;
}

int R_Proxy_get_symbol (char const* pSymbol,BDX_Data** pData)
{
  IoBuffer lBuffer;
  SEXP lSexp;
  SEXP lVar;
  ParseStatus lStatus;

  R_IoBufferInit (&lBuffer);
  R_IoBufferPuts ((char*) pSymbol,&lBuffer);
  R_IoBufferPuts ("\n",&lBuffer);

  /* don't generate code, just a try */
  R_IoBufferReadReset (&lBuffer);
  lSexp = R_Parse1Buffer (&lBuffer,0,&lStatus);

  if (lStatus == PARSE_OK)
    {
      /* now generate code */
      R_IoBufferReadReset (&lBuffer);
      lSexp = R_Parse1Buffer (&lBuffer,1,&lStatus);
      R_Visible = 0;
      R_EvalDepth = 0;
      PROTECT(lSexp);

      /* check for valid symbol... */
      if (TYPEOF (lSexp) != SYMSXP)
	{
	  RPROXY_TRACE(printf(">> %s is not a symbol\n",pSymbol));
	  UNPROTECT (1);
	  return SC_PROXY_ERR_INVALIDSYMBOL;
	}

      lVar = findVar (lSexp,R_GlobalEnv);

      if (lVar == R_UnboundValue)
	{
	  RPROXY_TRACE(printf(">> %s is an unbound value\n",pSymbol));
	  UNPROTECT (1);
	  return SC_PROXY_ERR_INVALIDSYMBOL;
	}
      {
	int lRc = SEXP2BDX(lVar,pData);
	UNPROTECT (1);

	if(lRc == 0) {
	  return SC_PROXY_OK;
	} else {
	  return SC_PROXY_ERR_UNSUPPORTEDTYPE;
	}
      }
    }
  return SC_PROXY_OK;
}

/* 04-02-19 | baier | don't PROTECT strings in a vector, new data structs */
/* 04-03-02 | baier | removed traces */
/* 04-10-15 | baier | no more BDX_VECTOR (only BDX_ARRAY) */
/* 05-05-16 | baier | use BDX2SEXP, clean-up */
int R_Proxy_set_symbol (char const* pSymbol,BDX_Data const* pData)
{
  SEXP lSymbol = 0;
  SEXP lData = 0;

  /*  RPROXY_TRACE(printf("calling BDX2SEXP\n")); */
  if(BDX2SEXP(pData,&lData) != 0) {
    /*    RPROXY_TRACE(printf("error BDX2SEXP\n")); */
    return SC_PROXY_ERR_UNSUPPORTEDTYPE;
  }
  /*  RPROXY_TRACE(printf("ok BDX2SEXP\n")); */

  /* install a new symbol or get the existing symbol */
  lSymbol = install ((char*) pSymbol);

  /* and set the data to the symbol */
  setVar(lSymbol,lData,R_GlobalEnv);

  return SC_PROXY_OK;
}

int R_Proxy_term ()
{
  end_Rmainloop();

  return SC_PROXY_OK;
}

