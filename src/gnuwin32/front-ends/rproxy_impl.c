/*
 *  RProxy: Connector implementation between application and R language
 *  Copyright (C) 1999--2001 Thomas Baier
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
 *  $Id: rproxy_impl.c,v 1.17 2002/04/30 19:13:56 ripley Exp $
 */

#define NONAMELESSUNION
#include <windows.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
//#include "globalvar.h"
//#undef CharacterMode
//#undef R_Interactive
#include <config.h>
#include <Rversion.h>
#include <Startup.h>
#include "bdx.h"
#include "SC_proxy.h"
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


int SEXP2BDX_Data (SEXP pExpression,BDX_Data** pData)
{
  BDX_Data* lData = 0;

  // allocate buffer
  lData = (BDX_Data*) malloc (sizeof (BDX_Data));
  *pData = lData;
  assert (*pData != NULL);

  // we support the following types at the moment
  //
  //  integer (scalar, vectors and arrays)
  //  real (scalars, vectors and arrays)
  //  logical (scalars, vectors and arrays)
  //  string (scalars, vectors and arrays)
  //  null
  //
  // we should support soon
  //
  //  complex vectors
  //  generic vectors
  switch (TYPEOF (pExpression))
    {
    case NILSXP	 :
      lData->type = BDX_NULL;

      // dimensions: 1
      lData->dim_count = 1;
      lData->dimensions =
	(BDX_Dimension*) malloc (sizeof (BDX_Dimension));
      lData->dimensions[0] = 0;

      // data: empty (just a dummy data record)
      lData->raw_data =
	(BDX_RawData*) malloc (sizeof (BDX_RawData));

      //      UNPROTECT (1);

      return SC_PROXY_OK;

      break;
    case LGLSXP	 :
      lData->type = BDX_BOOL;
      break;
    case INTSXP	 :
      lData->type = BDX_INT;
      break;
    case REALSXP	 :
      lData->type = BDX_DOUBLE;
      break;
    case STRSXP	 :
      lData->type = BDX_STRING;
      break;
      // case VECSXP	 : printf ("type: generic vectors\n");
      // break;
      // case CPLXSXP	 : printf ("type: complex variables\n");
      // break;
    default:
      //      UNPROTECT (1);
      free (lData);
      *pData = NULL;
      return SC_PROXY_ERR_UNSUPPORTEDTYPE;
    }

  // the type is set now. NULL values have already returned

  // is it a scalar, a vector or an array?

  // bug: no dimensions stored
  if (LENGTH (pExpression) == 0)
    {
      free (lData);
      *pData = NULL;
      return SC_PROXY_ERR_UNKNOWN;
    }

  // scalar: length 1
  if (LENGTH (pExpression) == 1)
    {
      lData->type |= BDX_SCALAR;
      lData->dim_count = 1;
      lData->dimensions =
	(BDX_Dimension*) malloc (sizeof (BDX_Dimension));
      lData->dimensions[0] = 1;
      lData->raw_data =
	(BDX_RawData*) malloc (sizeof (BDX_RawData));
      switch (lData->type & BDX_SMASK)
	{
	case BDX_BOOL:
	  lData->raw_data[0].bool_value = LOGICAL (pExpression)[0];
	  break;
	case BDX_INT:
	  lData->raw_data[0].int_value = INTEGER (pExpression)[0];
	  break;
	case BDX_DOUBLE:
	  lData->raw_data[0].double_value = REAL (pExpression)[0];
	  break;
	case BDX_STRING:
	  lData->raw_data[0].string_value = strdup (CHAR (STRING_ELT(pExpression, 0)));
	  // lData->raw_data[0].string_value = strdup (STRING_ELT(pExpression, 0));
	  break;
	}
      //      UNPROTECT (1);
    }
  else
    {
      // is it a vector or an array?
      SEXP lDimension;

      lDimension = getAttrib (pExpression,R_DimSymbol);

      PROTECT (lDimension);

      if (TYPEOF (lDimension) == NILSXP)
	{
	  // vector
	  int i;

	  lData->type |= BDX_VECTOR;
	  lData->dim_count = 1;
	  lData->dimensions =
	    (BDX_Dimension*) malloc (sizeof (BDX_Dimension));
	  lData->dimensions[0] = LENGTH (pExpression);
	  lData->raw_data =
	    (BDX_RawData*) malloc (sizeof (BDX_RawData)
				   * lData->dimensions[0]);

	  // copy the data
	  for (i = 0; i < lData->dimensions[0];i++)
	    {
	      switch (lData->type & BDX_SMASK)
		{
		case BDX_BOOL:
		  lData->raw_data[i].bool_value = LOGICAL (pExpression)[i];
		  break;
		case BDX_INT:
		  lData->raw_data[i].int_value = INTEGER (pExpression)[i];
		  break;
		case BDX_DOUBLE:
		  lData->raw_data[i].double_value = REAL (pExpression)[i];
		  break;
		case BDX_STRING:
		  lData->raw_data[i].string_value = strdup (CHAR (STRING_ELT(pExpression, i)));
		  // lData->raw_data[0].string_value = strdup (STRING_ELT(pExpression, 0));
		  break;
		}
	    }

	  UNPROTECT (1); // dimension
	  //	  UNPROTECT (1); // variable

	  return SC_PROXY_OK;
	}
      else
	{
	  // array with LENGTH(lDimension) dimensions
	  if (TYPEOF (lDimension) == INTSXP)
	    {
	      int i;
	      int lTotalSize = 1;

	      lData->type |= BDX_ARRAY;
	      lData->dim_count = LENGTH (lDimension);

	      lData->dimensions =
		(BDX_Dimension*) malloc (sizeof (BDX_Dimension)
					 * lData->dim_count);

	      // compute the total number of data elements
	      for (i = 0;i < lData->dim_count;i++)
		{
		  lData->dimensions[i] = INTEGER (lDimension)[i];
		  lTotalSize *= lData->dimensions[i];
		}

	      lData->raw_data =
		(BDX_RawData*) malloc (sizeof (BDX_RawData)
				       * lTotalSize);

	      // copy the data
	      for (i = 0; i < lTotalSize;i++)
		{
		  switch (lData->type & BDX_SMASK)
		    {
		    case BDX_BOOL:
		      lData->raw_data[i].bool_value = LOGICAL (pExpression)[i];
		      break;
		    case BDX_INT:
		      lData->raw_data[i].int_value = INTEGER (pExpression)[i];
		      break;
		    case BDX_DOUBLE:
		      lData->raw_data[i].double_value = REAL (pExpression)[i];
		      break;
		    case BDX_STRING:
		      //lData->raw_data[i].string_value = strdup ("test");
		      lData->raw_data[i].string_value = strdup (CHAR (STRING_ELT(pExpression, i)));
		      break;
		    }
		}

	      UNPROTECT (1); // dimension
	      //	      UNPROTECT (1); // variable

	      return SC_PROXY_OK;
	    }
	  else
	    {
	      // unknown error
	      free (lData);
	      *pData = NULL;

	      UNPROTECT (1); // dimension
	      //	      UNPROTECT (1); // variable

	      return SC_PROXY_ERR_UNKNOWN;
	    }
	}
    }

  return SC_PROXY_OK;
}

// 00-02-18 | baier | parse parameter string and fill parameter structure
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
      // NSIZE?
      if (strncmp (lParameterStart,"NSIZE=",6) == 0)
	{
	  lParameterStart += 6;

	  lPosOfSemicolon = strchr (lParameterStart,';');
	  lIndexOfSemicolon = lPosOfSemicolon - lParameterStart;

	  if (lPosOfSemicolon)
	    {
	      lTmpBuffer = malloc (lIndexOfSemicolon + 1); // to catch NSIZE=;
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
	      lTmpBuffer = malloc (lIndexOfSemicolon + 1); // to catch VSIZE=;
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

// 00-02-18 | baier | R_Proxy_init() now takes parameter string, parse it
int R_Proxy_init (char const* pParameterString)
{
  structRstart rp;
  Rstart Rp = &rp;
  char Rversion[25];
  static char RUser[MAX_PATH], RHome[MAX_PATH]; // BR
  char *p;

  sprintf(Rversion, "%s.%s", R_MAJOR, R_MINOR);
  if(strcmp(getDLLVersion(), Rversion) != 0) {
    fprintf(stderr, "Error: R.DLL version does not match\n");
    return SC_PROXY_ERR_UNKNOWN;
  }

  R_DefParams(Rp);

  // first, try process-local environment space (CRT)
  if (getenv("R_HOME")) {
      strcpy(RHome, getenv("R_HOME"));
  } else {
      // get variable from process-local environment space (Windows API)
      if (GetEnvironmentVariable ("R_HOME", RHome, sizeof (RHome)) == 0) {
	  // not found, fall back to getRHOME()
	  strcpy(RHome, getRHOME());
      }
    }

  Rp->rhome = RHome;
  /*
   * try R_USER then HOME then working directory
   */
  if (getenv("R_USER")) {
    strcpy(RUser, getenv("R_USER")); // BR
  } else if (getenv("HOME")) {
      strcpy(RUser, getenv("HOME"));
  } else if (getenv("HOMEDRIVE")) { // BR
      strcpy(RUser, getenv("HOMEDRIVE"));
      strcat(RUser, getenv("HOMEPATH"));
  } else
      GetCurrentDirectory(MAX_PATH, RUser);
  p = RUser + (strlen(RUser) - 1); // BR
  if (*p == '/' || *p == '\\') *p = '\0'; // BR
  Rp->home = RUser;
  Rp->CharacterMode = LinkDLL;
  Rp->ReadConsole = R_Proxy_ReadConsole;
  Rp->WriteConsole = R_Proxy_WriteConsole;
  Rp->CallBack = R_Proxy_CallBack;
  Rp->message = R_Proxy_askok;

  Rp->yesnocancel = R_Proxy_askyesnocancel;
  Rp->busy = R_Proxy_Busy;
  Rp->R_Quiet = 1;
#if 1
  // run as "interactive", so server won't be killed after an error
  Rp->R_Slave = Rp->R_Verbose = 0;
  Rp->R_Interactive = 1;
#else
  Rp->R_Slave = Rp->R_Interactive = Rp->R_Verbose = 0;
#endif
  Rp->RestoreAction = 0; /* no restore */
  Rp->SaveAction = 2;    /* no save */
  Rp->CommandLineArgs = NULL;
  Rp->NumCommandLineArgs = 0;

/*  Rp->nsize = 300000;
    Rp->vsize = 6e6;*/
  R_SetParams(Rp); /* so R_ShowMessage is set */
  R_SizeFromEnv(Rp);

  // parse parameters
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

// 01-06-05 | baier | SETJMP and fatal error handling around eval()
int R_Proxy_evaluate (char const* pCmd,BDX_Data** pData)
{
  SEXP rho = R_GlobalEnv;
  IoBuffer lBuffer;
  SEXP lSexp;
  int lRc;
  int lStatus;
  SEXP lResult;

  // for SETJMP/LONGJMP
  s_EvalInProgress = 0;

  R_IoBufferInit (&lBuffer);
  R_IoBufferPuts ((char*) pCmd,&lBuffer);
  R_IoBufferPuts ("\n",&lBuffer);

  // don't generate code, just a try
  R_IoBufferReadReset (&lBuffer);
  lSexp = R_Parse1Buffer (&lBuffer,0,&lStatus);

  switch (lStatus)
    {
    case PARSE_NULL:
      // we forget the IoBuffer "lBuffer", so don't do anything here
      lRc = SC_PROXY_ERR_PARSE_INVALID;
      break;
    case PARSE_OK:
      // now generate code
      R_IoBufferReadReset (&lBuffer);
      lSexp = R_Parse1Buffer (&lBuffer,1,&lStatus);
      R_Visible = 0;
      R_EvalDepth = 0;
      PROTECT(lSexp);
      {
	SETJMP (R_Toplevel.cjmpbuf);
	R_GlobalContext = R_ToplevelContext = &R_Toplevel;

	if (!s_EvalInProgress)
	  {
	    s_EvalInProgress = 1;
	    lResult = eval (lSexp,rho);
	    s_EvalInProgress = 0;
	  }
	else
	  {
	    return SC_PROXY_ERR_EVALUATE_STOP;
	  }
      }
      lRc = SEXP2BDX_Data (lResult,pData);
      // no last value
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
      // never reached
      lRc = SC_PROXY_ERR_UNKNOWN;
      break;
    }

  return lRc;
}

// 01-06-05 | baier | SETJMP and fatal error handling around eval()
int R_Proxy_evaluate_noreturn (char const* pCmd)
{
  SEXP rho = R_GlobalEnv;
  IoBuffer lBuffer;
  SEXP lSexp;
  int lRc;
  int lStatus;

  // for SETJMP/LONGJMP
  s_EvalInProgress = 0;

  R_IoBufferInit (&lBuffer);
  R_IoBufferPuts ((char*) pCmd,&lBuffer);
  R_IoBufferPuts ("\n",&lBuffer);

  // don't generate code, just a try
  R_IoBufferReadReset (&lBuffer);
  lSexp = R_Parse1Buffer (&lBuffer,0,&lStatus);

  switch (lStatus)
    {
    case PARSE_NULL:
      // we forget the IoBuffer "lBuffer", so don't do anything here
      lRc = SC_PROXY_ERR_PARSE_INVALID;
      break;
    case PARSE_OK:
      // now generate code
      R_IoBufferReadReset (&lBuffer);
      lSexp = R_Parse1Buffer (&lBuffer,1,&lStatus);
      R_Visible = 0;
      R_EvalDepth = 0;
      PROTECT(lSexp);
      // at the moment, discard the result of the eval
      {
	SETJMP (R_Toplevel.cjmpbuf);
	R_GlobalContext = R_ToplevelContext = &R_Toplevel;

	if (!s_EvalInProgress)
	  {
	    s_EvalInProgress = 1;
	    eval (lSexp,rho);
	    s_EvalInProgress = 0;
	  }
	else
	  {
	    return SC_PROXY_ERR_EVALUATE_STOP;
	  }
      }
      // no last value
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
      // never reached
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
  int lStatus;

  R_IoBufferInit (&lBuffer);
  R_IoBufferPuts ((char*) pSymbol,&lBuffer);
  R_IoBufferPuts ("\n",&lBuffer);

  // don't generate code, just a try
  R_IoBufferReadReset (&lBuffer);
  lSexp = R_Parse1Buffer (&lBuffer,0,&lStatus);

  if (lStatus == PARSE_OK)
    {
      // now generate code
      R_IoBufferReadReset (&lBuffer);
      lSexp = R_Parse1Buffer (&lBuffer,1,&lStatus);
      R_Visible = 0;
      R_EvalDepth = 0;
      PROTECT(lSexp);

      // check for valid symbol...
      if (TYPEOF (lSexp) != SYMSXP)
	{
	  printf (">> %s is not a symbol\n",pSymbol);
	  UNPROTECT (1);
	  return SC_PROXY_ERR_INVALIDSYMBOL;
	}

      lVar = findVar (lSexp,R_GlobalEnv);

      if (lVar == R_UnboundValue)
	{
	  printf (">> %s is an unbound value\n",pSymbol);
	  UNPROTECT (1);
	  return SC_PROXY_ERR_INVALIDSYMBOL;
	}

      {
	int lRc = SEXP2BDX_Data (lVar,pData);
	UNPROTECT (1);

	return lRc;
      }
    }

  return SC_PROXY_OK;
}

int R_Proxy_set_symbol (char const* pSymbol,BDX_Data const* pData)
{
  SEXP lSymbol = 0;
  SEXP lData = 0;
  int lProtectCount = 1;
  int lRet = SC_PROXY_OK;

  switch (pData->type & BDX_CMASK)
    {
      // scalar?
    case BDX_SCALAR:
      {
	switch (pData->type & BDX_SMASK)
	  {
	  case BDX_BOOL:
	    lData = PROTECT (allocVector (LGLSXP,1));
	    LOGICAL(lData)[0] = pData->raw_data[0].bool_value;
	    break;
	  case BDX_INT:
	    lData = PROTECT (allocVector (INTSXP,1));
	    INTEGER(lData)[0] = pData->raw_data[0].int_value;
	    break;
	  case BDX_DOUBLE:
	    lData = PROTECT (allocVector (REALSXP,1));
	    REAL(lData)[0] = pData->raw_data[0].double_value;
	    break;
	  case BDX_STRING:
	    {
	      SEXP lStringSExp =
		allocString (strlen (pData->raw_data[0].string_value));
	      PROTECT (lStringSExp); lProtectCount++;
	      strcpy (CHAR(lStringSExp),pData->raw_data[0].string_value);
	      lData = PROTECT (allocVector (STRSXP,1));
	      SET_STRING_ELT(lData, 0, lStringSExp);
	    }
	    break;
	  default:
	    lRet = SC_PROXY_ERR_UNSUPPORTEDTYPE;
	  }
      }

      break;
      // vectors or arrays
    case BDX_VECTOR:
    case BDX_ARRAY:
      {
	// allocate a dimensions vector
	SEXP lDimensions;
	unsigned int i;
	unsigned int lTotalSize = 1;

	PROTECT (lDimensions = allocVector (INTSXP,pData->dim_count));
	lProtectCount++;

	for (i = 0;i < pData->dim_count;i++)
	  {
	    INTEGER (lDimensions)[i] = pData->dimensions[i];
	    lTotalSize *= pData->dimensions[i];
	  }

	switch (pData->type & BDX_SMASK)
	  {
	  case BDX_BOOL:
	    lData = PROTECT (allocVector (LGLSXP,lTotalSize));
	    setAttrib (lData,R_DimSymbol,lDimensions);

	    for (i = 0;i < lTotalSize;i++)
	      {
		LOGICAL(lData)[i] = pData->raw_data[i].bool_value;
	      }
	    break;
	  case BDX_INT:
	    lData = PROTECT (allocVector (INTSXP,lTotalSize));
	    setAttrib (lData,R_DimSymbol,lDimensions);

	    for (i = 0;i < lTotalSize;i++)
	      {
		INTEGER(lData)[i] = pData->raw_data[i].int_value;
	      }
	    break;
	  case BDX_DOUBLE:
	    lData = PROTECT (allocVector (REALSXP,lTotalSize));
	    setAttrib (lData,R_DimSymbol,lDimensions);

	    for (i = 0;i < lTotalSize;i++)
	      {
		REAL(lData)[i] = pData->raw_data[i].double_value;
	      }
	    break;
	  case BDX_STRING:
	    {
	      lData = PROTECT (allocVector (STRSXP,lTotalSize));
	      setAttrib (lData,R_DimSymbol,lDimensions);

	      for (i = 0;i < lTotalSize;i++)
		{
		  SEXP lStringSExp;
		  lStringSExp =
		    allocString (strlen (pData->raw_data[i].string_value));
		  PROTECT (lStringSExp); lProtectCount++;
		  strcpy (CHAR(lStringSExp),pData->raw_data[i].string_value);
		  SET_STRING_ELT(lData, i, lStringSExp);
		}
	    }
	    break;
	  default:
	    lRet = SC_PROXY_ERR_UNSUPPORTEDTYPE;
	  }
      }
      break;
    default:
      lRet = SC_PROXY_ERR_UNSUPPORTEDTYPE;
    }

  if (lRet != SC_PROXY_OK)
    {
      return lRet;
    }

  // install a new symbol or get the existing symbol
  lSymbol = install ((char*) pSymbol);

  // and set the data to the symbol
  setVar(lSymbol,lData,R_GlobalEnv);

  UNPROTECT (lProtectCount);

  return SC_PROXY_OK;
}

int R_Proxy_term ()
{
  end_Rmainloop();

  return SC_PROXY_OK;
}

