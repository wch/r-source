/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999  R Development Core Team
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

#include <windows.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "Rconfig.h"
#include "Rversion.h"
#include "Startup.h"
#include "bdx.h"
#include "rproxy.h"
#include "rproxy_impl.h"
#include "IOStuff.h"
#include "Parse.h"

#define R_GlobalEnv (*__imp_R_GlobalEnv)
#define R_Visible (*__imp_R_Visible)
#define R_EvalDepth (*__imp_R_EvalDepth)
#define R_DimSymbol (*__imp_R_DimSymbol)

extern SEXP R_GlobalEnv;
extern int R_Visible;
extern int R_EvalDepth;
extern SEXP R_DimSymbol;


/* calls into the R DLL */
extern char *getDLLVersion();
extern void R_DefParams(Rstart);
extern void R_SetParams(Rstart);
extern void setup_term_ui(void);
extern char *getRHOME();
extern void setup_Rmainloop(), end_Rmainloop(), R_ReplDLLinit();
extern void askok(char *);

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
}

void R_Proxy_CallBack()
{
    /* called during i/o, eval, graphics in ProcessEvents */
}

void R_Proxy_Busy(int which)
{
    /* set a busy cursor ... in which = 1, unset if which = 0 */
}


int R_Proxy_init ()
{
    structRstart rp;
    Rstart Rp = &rp;
    char Rversion[25];
    static char RUser[MAX_PATH], RHome[MAX_PATH];
    char *p;

    sprintf(Rversion, "%s.%s", R_MAJOR, R_MINOR);
    if(strcmp(getDLLVersion(), Rversion) != 0) {
	fprintf(stderr, "Error: R.DLL version does not match\n");
	return R_PROXY_ERR_UNKNOWN;
    }

    R_DefParams(Rp);
    if(getenv("R_HOME")) {
	strcpy(RHome, getenv("R_HOME"));
    } else {
	strcpy(RHome, getRHOME());
    }
    Rp->rhome = RHome;
/*
 * try R_USER then HOME then working directory
 */
    if (getenv("R_USER")) {
	strcpy(RUser, getenv("R_USER"));
    } else if (getenv("HOME")) {
	strcpy(RUser, getenv("HOME"));
    } else if (getenv("HOMEDIR")) {
	strcpy(RUser, getenv("HOMEDIR"));
	strcat(RUser, getenv("HOMEPATH"));
    } else
	GetCurrentDirectory(MAX_PATH, RUser);
    p = RUser + (strlen(RUser) - 1);
    if (*p == '/' || *p == '\\') *p = '\0';
    Rp->home = RUser;
    Rp->CharacterMode = LinkDLL;
    Rp->ReadConsole = R_Proxy_ReadConsole;
    Rp->WriteConsole = R_Proxy_WriteConsole;
    Rp->CallBack = R_Proxy_CallBack;
    Rp->message = R_Proxy_askok;
                                                                               
    Rp->yesnocancel = R_Proxy_askyesnocancel;
    Rp->busy = R_Proxy_Busy;
    Rp->R_Quiet = 1;
    Rp->R_Slave = Rp->R_Interactive = Rp->R_Verbose = 0;
    Rp->RestoreAction = 0; /* no restore */
    Rp->SaveAction = 2;    /* no save */
/*  Rp->nsize = 300000;
    Rp->vsize = 6e6;*/
    R_SetParams(Rp); /* so R_ShowMessage is set */
    R_SizeFromEnv(Rp);
    R_SetParams(Rp);
 
  
    setup_term_ui();
    setup_Rmainloop();
    R_ReplDLLinit();

    return R_PROXY_OK;
}

int R_Proxy_eval (char const* pCmd)
{
  SEXP rho = R_GlobalEnv;
  IoBuffer lBuffer;
  SEXP lSexp;
  int lRc;
  int lStatus;

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
      lRc = R_PROXY_ERR_PARSE_INVALID;
      break;
    case PARSE_OK:
	/* now generate code */
      R_IoBufferReadReset (&lBuffer);
      lSexp = R_Parse1Buffer (&lBuffer,1,&lStatus);
      R_Visible = 0;
      R_EvalDepth = 0;
      PROTECT(lSexp);
      /*      R_Busy(1); */
      /* at the moment, discard the result of the eval */
      eval (lSexp,rho);
      /* R_CurrentExpr = eval(lSexp, rho); 
	 no last value
      SYMVALUE(R_LastvalueSymbol) = R_CurrentExpr; */
      UNPROTECT(1);
      /*
	if (R_Visible)
	PrintValueEnv(R_CurrentExpr, rho);
	if (R_CollectWarnings)
	PrintWarnings();
        R_Busy(0); */
      lRc = R_PROXY_OK;
      break;
    case PARSE_ERROR:
      lRc = R_PROXY_ERR_PARSE_INVALID;
      break;
    case PARSE_INCOMPLETE:
      lRc = R_PROXY_ERR_PARSE_INCOMPLETE;
      break;
    case PARSE_EOF:
      lRc = R_PROXY_ERR_PARSE_INVALID;
      break;
    default:
	/* never reached */
      lRc = R_PROXY_ERR_UNKNOWN;
      break;
    }

  return lRc;
}

int R_Proxy_get_symbol (char const* pSymbol,BDX_Data** pData)
{
    //  int c, status;
    //  SEXP rho = R_GlobalEnv;
    IoBuffer lBuffer;
    SEXP lSexp;
    SEXP lVar;
    //  int lRc;
    int lStatus;
    BDX_Data* lData = NULL;

    R_IoBufferInit (&lBuffer);
    R_IoBufferPuts ((char*) pSymbol, &lBuffer);
    R_IoBufferPuts ("\n", &lBuffer);

    // don't generate code, just a try
    R_IoBufferReadReset (&lBuffer);
    lSexp = R_Parse1Buffer (&lBuffer, 0, &lStatus);

    if (lStatus == PARSE_OK) {
	// now generate code
	R_IoBufferReadReset (&lBuffer);
	lSexp = R_Parse1Buffer (&lBuffer, 1, &lStatus);
	R_Visible = 0;
	R_EvalDepth = 0;
	PROTECT(lSexp);

	// check for valid symbol...
	if (TYPEOF (lSexp) != SYMSXP) {
	    printf (">> %s is not a symbol\n", pSymbol);
	    UNPROTECT (1);
	    return R_PROXY_ERR_INVALIDSYMBOL;
	}

	lVar = findVar (lSexp,R_GlobalEnv);

	if (lVar == R_UnboundValue) {
	    printf (">> %s is an unbound value\n", pSymbol);
	    UNPROTECT (1);
	    return R_PROXY_ERR_INVALIDSYMBOL;
	}

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
	switch (TYPEOF (lVar)) {
	case NILSXP	 :
	    printf (">> %s is a NULL value\n", pSymbol);

	    lData->type = BDX_NULL;

	    // dimensions: 1
	    lData->dim_count = 1;
	    lData->dimensions =
		(BDX_Dimension*) malloc (sizeof (BDX_Dimension));
	    lData->dimensions[0] = 0;

	    // data: empty (just a dummy data record)
	    lData->raw_data =
		(BDX_RawData*) malloc (sizeof (BDX_RawData));

	    UNPROTECT (1);

	    return R_PROXY_OK;

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
	    printf (">> cannot handle symbol %s of type %d\n",
		    pSymbol,TYPEOF (lVar));
	    UNPROTECT (1);
	    free (lData);
	    *pData = NULL;
	    return R_PROXY_ERR_UNSUPPORTEDTYPE;
	}

	// the type is set now. NULL values have already returned

	// is it a scalar, a vector or an array?

	// bug: no dimensions stored
	if (LENGTH (lVar) == 0) {
	    free (lData);
	    *pData = NULL;
	    return R_PROXY_ERR_UNKNOWN;
	}

	// scalar: length 1
	if (LENGTH (lVar) == 1) {
	    lData->type |= BDX_SCALAR;
	    lData->dim_count = 1;
	    lData->dimensions =
		(BDX_Dimension*) malloc (sizeof (BDX_Dimension));
	    lData->dimensions[0] = 1;
	    lData->raw_data =
		(BDX_RawData*) malloc (sizeof (BDX_RawData));
	    switch (lData->type & BDX_SMASK) {
	    case BDX_BOOL:
		lData->raw_data[0].bool_value = LOGICAL (lVar)[0];
		break;
	    case BDX_INT:
		lData->raw_data[0].int_value = INTEGER (lVar)[0];
		break;
	    case BDX_DOUBLE:
		lData->raw_data[0].double_value = REAL (lVar)[0];
		break;
	    case BDX_STRING:
		lData->raw_data[0].string_value = strdup (CHAR (STRING (lVar)[0]));
		// lData->raw_data[0].string_value = strdup (STRING (lVar)[0]);
		break;
	    }
	    UNPROTECT (1);
	} else {
	    // is it a vector or an array?
	    SEXP lDimension;

	    lDimension = getAttrib (lVar,R_DimSymbol);

	    PROTECT (lDimension);

	    if (TYPEOF (lDimension) == NILSXP) {
		// vector
		int i;

		printf (">> %s is a vector of length %d\n",
			pSymbol, LENGTH (lVar));

		lData->type |= BDX_VECTOR;
		lData->dim_count = 1;
		lData->dimensions =
		    (BDX_Dimension*) malloc (sizeof (BDX_Dimension));
		lData->dimensions[0] = LENGTH (lVar);
		lData->raw_data =
		    (BDX_RawData*) malloc (sizeof (BDX_RawData)
					   * lData->dimensions[0]);

		// copy the data
		for (i = 0; i < lData->dimensions[0];i++) {
		    switch (lData->type & BDX_SMASK) {
		    case BDX_BOOL:
			lData->raw_data[i].bool_value = LOGICAL (lVar)[i];
			break;
		    case BDX_INT:
			lData->raw_data[i].int_value = INTEGER (lVar)[i];
			break;
		    case BDX_DOUBLE:
			lData->raw_data[i].double_value = REAL (lVar)[i];
			break;
		    case BDX_STRING:
			lData->raw_data[i].string_value = strdup (CHAR (STRING (lVar)[i]));
			// lData->raw_data[0].string_value = strdup (STRING (lVar)[0]);
			break;
		    }
		}

		UNPROTECT (1); // dimension
		UNPROTECT (1); // variable

		return R_PROXY_OK;
	    } else {
		// array with LENGTH(lDimension) dimensions
		if (TYPEOF (lDimension) == INTSXP) {
		    int i;
		    int lTotalSize = 1;

		    lData->type |= BDX_ARRAY;
		    lData->dim_count = LENGTH (lDimension);

		    lData->dimensions =
			(BDX_Dimension*) malloc (sizeof (BDX_Dimension)
						 * lData->dim_count);

		    // compute the total number of data elements
		    for (i = 0;i < lData->dim_count;i++) {
			lData->dimensions[i] = INTEGER (lDimension)[i];
			lTotalSize *= lData->dimensions[i];
		    }

		    lData->raw_data =
			(BDX_RawData*) malloc (sizeof (BDX_RawData)
					       * lTotalSize);

		    // copy the data
		    for (i = 0; i < lTotalSize;i++) {
			switch (lData->type & BDX_SMASK) {
			case BDX_BOOL:
			    lData->raw_data[i].bool_value = LOGICAL (lVar)[i];
			    break;
			case BDX_INT:
			    lData->raw_data[i].int_value = INTEGER (lVar)[i];
			    break;
			case BDX_DOUBLE:
			    lData->raw_data[i].double_value = REAL (lVar)[i];
			    break;
			case BDX_STRING:
			    lData->raw_data[i].string_value = strdup ("test");
			    // lData->raw_data[0].string_value = strdup (STRING (lVar)[0]);
			    break;
			}
		    }

		    UNPROTECT (1); // dimension
		    UNPROTECT (1); // variable

		    return R_PROXY_OK;
		} else {
		    // unknown error
		    free (lData);
		    *pData = NULL;

		    UNPROTECT (1); // dimension
		    UNPROTECT (1); // variable

		    return R_PROXY_ERR_UNKNOWN;
		}
	    }
	}
    }

    return R_PROXY_OK;
}

int R_Proxy_term ()
{
    end_Rmainloop();
    return R_PROXY_OK;
}

