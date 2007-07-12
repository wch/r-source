/*******************************************************************************
 *  BDX: Binary Data eXchange format library
 *  Copyright (C) 1999-2006 Thomas Baier
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
 *  Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *  MA 02110-1301, USA.
 *
 *  Conversion functions from SEXP to BDX and vice versa.
 *
 ******************************************************************************/

#ifndef  _BDX_SEXP_H_
#include "bdx_SEXP.h"
#endif

#include "bdx.h"
#include "bdx_util.h"
#include "Rinternals.h"
/*#include "rproxy_impl.h" */
#include "com_util.h"
#include <stdio.h>
#include <assert.h>

/*
 * prototypes for internal helper functions
 */
static int EXTPTRSXP2LPSTREAM(RCOM_OBJHANDLE pHandle,
			      LPSTREAM* pStream);
#define getSpecialValueFromLogical(x) getSpecialValueFromInteger((x))
static unsigned long getSpecialValueFromInteger(int pSEXPVal);
static unsigned long getSpecialValueFromDouble(double pSEXPVal);
static double getDoubleFromSpecialValue(unsigned long pSpecialValue);

#define BDX_DM_DEFAULT    0UL
#define BDX_DM_UNDEFINED  ((unsigned long) -1)
#define BDX_DM_NO_SPECIAL 1UL

static unsigned long s_data_mode = BDX_DM_UNDEFINED;

/* ==========================================================================
   Implementation
   ========================================================================== */
/* 05-05-19 | baier | handle BDX_GENERIC */
int BDX2SEXP(BDX_Data const* pBDXData,SEXP* pSEXPData)
{
  SEXP lData = NULL;
  SEXP lDimensions = NULL;
  int lTotalSize = 1;
  int lProtectCount = 0;
  int i;

  assert(pSEXPData != NULL);

  switch(pBDXData->type & BDX_CMASK) {
  case BDX_SCALAR:
    /* no DIM attribute */
    break;
  case BDX_ARRAY:
    /* create DIM attribute */
    PROTECT(lDimensions = allocVector(INTSXP,pBDXData->dim_count));
    lProtectCount++;
    for (i = 0;i < pBDXData->dim_count;i++) {
      INTEGER (lDimensions)[i] = pBDXData->dimensions[i];
      lTotalSize *= pBDXData->dimensions[i];
    }
    break;
  default:
    /* unknown/unsupported type */
    UNPROTECT(lProtectCount);
    return -1;
  }

  /* now allocate the SEXP and copy the data */
  switch(pBDXData->type & BDX_SMASK) {
  case BDX_BOOL:
    lData = PROTECT(allocVector(LGLSXP,lTotalSize));
    lProtectCount++;
    for(i = 0;i < lTotalSize;i++) {
      LOGICAL(lData)[i] = pBDXData->data.raw_data[i].bool_value;
    }
    break;
  case BDX_INT:
    lData = PROTECT(allocVector(INTSXP,lTotalSize));
    lProtectCount++;
    for(i = 0;i < lTotalSize;i++) {
      INTEGER(lData)[i] = pBDXData->data.raw_data[i].int_value;
    }
    break;
  case BDX_DOUBLE:
    lData = PROTECT(allocVector(REALSXP,lTotalSize));
    lProtectCount++;
    for(i = 0;i < lTotalSize;i++) {
      REAL(lData)[i] = pBDXData->data.raw_data[i].double_value;
    }
    break;
  case BDX_STRING:
    lData = PROTECT(allocVector(STRSXP,lTotalSize));
    lProtectCount++;
    for(i = 0;i < lTotalSize;i++)
      SET_STRING_ELT(lData, i, mkChar(pBDXData->data.raw_data[i].string_value));
    break;
  case BDX_POINTER:
    /* BDX_POINTER not supported now, trace the contents */
    BDX_ERR(printf("BDX_POINTER found, value is %p\n",
		   pBDXData->data.raw_data[0].ptr));
    *pSEXPData = NULL;
    UNPROTECT(lProtectCount);
    return -1;
  case BDX_HANDLE:
    /* only single COM object supported (no arrays) */
    if((pBDXData->type & BDX_CMASK) != BDX_SCALAR) {
      BDX_ERR(printf("array of BDX_HANDLE found. Not supported\n"));
      *pSEXPData = NULL;
      UNPROTECT(lProtectCount);
      return -1;
    }

    /* marshalled COM object as an LPSTREAM:
     *
     * 1. unmarshal the object from the stream (IDispatch) using
     *    CoGetInterfaceAndReleaseStream()
     * 2. COM object must be Release()d afterwards
     * 3. Stream still valid. Must be released afterwards
     */
    {
      LPSTREAM lStream = pBDXData->data.raw_data[0].ptr;
      LPDISPATCH lDispatch;
      RCOM_OBJHANDLE lHandle;
#if 1
      HRESULT lRc = CoUnmarshalInterface(lStream,&IID_IDispatch,
					 (void*) &lDispatch);
#else
      HRESULT lRc = CoGetInterfaceAndReleaseStream(lStream,&IID_IDispatch,
						   (void*) &lDispatch);
#endif
      if(FAILED(lRc)) {
	BDX_ERR(printf("unmarshalling stream ptr %p failed with hr=%08x\n",
			  lStream,lRc));
	*pSEXPData = NULL;
	UNPROTECT(lProtectCount);
	return -1;
      }

      /* create SEXP for COM object */
      lHandle = com_addObject (lDispatch);
      lData = com_createSEXP(lHandle);
    }
    break;
  case BDX_SPECIAL:
    lData = PROTECT (allocVector (REALSXP,lTotalSize));
    lProtectCount++;
    for(i = 0;i < lTotalSize;i++) {
      REAL(lData)[i] =
	getDoubleFromSpecialValue(pBDXData->data.raw_data[i].special_value);
    }
    break;
  case BDX_GENERIC:
    /* generic array */
    lData = PROTECT(allocVector(VECSXP,lTotalSize));
    lProtectCount++;
    for(i = 0;i < lTotalSize;i++) {
      SEXP lSEXP = NULL;
      /* handle contents */
      switch(pBDXData->data.raw_data_with_type[i].type & BDX_SMASK) {
      case BDX_BOOL:
	lSEXP = allocVector(LGLSXP,1);
	LOGICAL(lSEXP)[0] =
	  pBDXData->data.raw_data_with_type[i].raw_data.bool_value;
	break;
      case BDX_INT:
	lSEXP = allocVector(INTSXP,1);
	INTEGER(lSEXP)[0] =
	  pBDXData->data.raw_data_with_type[i].raw_data.int_value;
	break;
      case BDX_DOUBLE:
	lSEXP = allocVector(REALSXP,1);
	REAL(lSEXP)[0] =
	  pBDXData->data.raw_data_with_type[i].raw_data.double_value;
	break;
      case BDX_STRING:
        lSEXP = mkString(pBDXData->data.raw_data_with_type[i].raw_data.string_value);
	break;
      case BDX_SPECIAL:
	lSEXP = allocVector(REALSXP,1);
	REAL(lSEXP)[0] =
	  getDoubleFromSpecialValue(pBDXData->data.raw_data_with_type[i].raw_data.special_value);
	break;
      default:
	lSEXP = allocVector(REALSXP,1);
	REAL(lSEXP)[0] = 
	  getDoubleFromSpecialValue(BDX_SV_NULL);
	BDX_ERR(printf("unknown BDX type %d in generic vector element %d, using NA\n",
		       pBDXData->data.raw_data_with_type[i].type & BDX_SMASK,
		       i));
      }
      SET_VECTOR_ELT(lData,i,lSEXP);
    }
    break;
  default:
    /* unknown/unsupported BDX type */
    *pSEXPData = NULL;
    BDX_ERR(printf("unknown BDX type %08x, (SMASK %08x)\n",
		      pBDXData->type,pBDXData->type & BDX_SMASK));
    UNPROTECT(lProtectCount);
    return -1;
  }

  if(lDimensions) {
    setAttrib (lData,R_DimSymbol,lDimensions);
  }

  *pSEXPData = lData;
  UNPROTECT(lProtectCount);

  return 0;
}

/* 05-05-24 | baier | VECSXP */
/* 05-06-05 | baier | support for special values (R_NaN,...), generic vectors */
/* 05-06-08 | baier | BDX_SPECIAL for scalars (REALSXP conversions) */
/* 06-02-15 | baier | fixes for COM objects/EXTPTRSXP */
/* 06-06-18 | baier | special values also for LOGICAL and INTEGER */
int SEXP2BDX(struct SEXPREC const* pSexp,BDX_Data** ppBDXData)
{
  BDX_Data* lData;
  SEXP lDimension;
  int lTotalSize = 1;
  int i;
  int lGeneric = 0;
  SEXP sexp = (SEXP) pSexp; /* to get rid of const/non-const warning */
  double lDoubleValue;
  int lLogicalValue;
  int lIntegerValue;

  assert(ppBDXData != NULL);

  *ppBDXData = 0;
  lData = bdx_alloc();

  /*
   * we support the following types at the moment
   *
   *  integer (scalar, vectors and arrays)
   *  real (scalars, vectors and arrays)
   *  logical (scalars, vectors and arrays)
   *  string (scalars, vectors and arrays)
   *  COM objects (IDispatch)
   *  null
   *
   * we should support soon
   *
   *  complex vectors
   *  generic vectors
   * bug: no dimensions stored
   */

  /*
   * zero-length SEXP: this is an error!
   */
  if (LENGTH(sexp) == 0) {
    /* empty/undefined symbol */
    BDX_ERR(printf("SEXP2BDX: SEXP has length 0\n"));
    bdx_free(lData);
    return -6;
  }

  /*
   * find out if the data type is supported
   */
  switch(TYPEOF(sexp)) {
  case NILSXP:
    lData->type |= BDX_SPECIAL;
    break;
  case LGLSXP:
    lData->type |= BDX_BOOL;
    break;
  case INTSXP:
    lData->type |= BDX_INT;
    break;
  case REALSXP:
    lData->type |= BDX_DOUBLE;
    break;
  case EXTPTRSXP:
    lData->type |= BDX_HANDLE;
    break;
  case STRSXP:
    lData->type |= BDX_STRING;
    break;
  case VECSXP:
    /* generic vectors */
    lData->type |= BDX_GENERIC;
    lGeneric = 1;
    break;
  default:
    bdx_free(lData);
    BDX_TRACE(printf("SEXP2BDX: unsupported SEXP type %d\n",
		      TYPEOF(sexp)));
    return -6;
  }

  /*
   * vector/matrix/scalar
   */
  lDimension = getAttrib (sexp,R_DimSymbol);
  PROTECT (lDimension);

  /* get dimension infos, allocate BDX_Dimension[] array */
  if(TYPEOF(lDimension) == INTSXP) {
    /* DIM atribute -> matrix */
    lData->type |= BDX_ARRAY;
    lData->dim_count = LENGTH(lDimension);
    lData->dimensions = (BDX_Dimension*) calloc(lData->dim_count,
						sizeof(BDX_Dimension));
    for (i = 0;i < lData->dim_count;i++) {
      lData->dimensions[i] = INTEGER(lDimension)[i];
      lTotalSize *= lData->dimensions[i];
    }
    /* lData->data.raw_data = (BDX_RawData*) malloc(sizeof(BDX_RawData)); */
  } else if((LENGTH(sexp) == 1) 
	    || (TYPEOF(sexp) == EXTPTRSXP)){
    /* scalar */
    lData->type |= BDX_SCALAR;
    lData->dim_count = 1;
    lData->dimensions = (BDX_Dimension*) malloc(sizeof(BDX_Dimension));
    lData->dimensions[0] = 1;
  } else if(TYPEOF(lDimension) == NILSXP) {
    /* no DIM atribute -> vector (== one-dimensional array) */
    lData->type |= BDX_ARRAY;
    lData->dim_count = 1;
    lData->dimensions = (BDX_Dimension*) malloc(sizeof(BDX_Dimension));
    lData->dimensions[0] = LENGTH(sexp);
    lTotalSize = lData->dimensions[0];
  }

  UNPROTECT(1); /* unlock lDimension */

  if(!lGeneric) {
    /* allocate memory for the data: BDX_RawData or BDX_RawDataWithType */
    lData->data.raw_data = (BDX_RawData*) calloc(lTotalSize,
						 sizeof(BDX_RawData));

    for(i = 0;(i < lTotalSize) && !lGeneric;i++) {
      switch(TYPEOF(sexp)) {
      case NILSXP:
	lData->data.raw_data[i].special_value = BDX_SV_NULL;
	break;
      case LGLSXP:
	/* check special values: R_NaInt */
	lLogicalValue = LOGICAL(sexp)[i];
	if(getSpecialValueFromLogical(lLogicalValue) != BDX_SV_UNK) {
	  lGeneric = 1;
	} else {
	  lData->data.raw_data[i].bool_value = lLogicalValue;
	}
	break;
      case INTSXP:
	/* check special values: R_NaInt */
	lIntegerValue = INTEGER(sexp)[i];
	if(getSpecialValueFromInteger(lIntegerValue) != BDX_SV_UNK) {
	  lGeneric = 1;
	} else {
	  lData->data.raw_data[i].int_value = lIntegerValue;
	}
	break;
      case REALSXP:
	/* check special values: R_NaReal, R_PosInf, R_NaN, R_NegInf */
	lDoubleValue = REAL(sexp)[i];
	if(getSpecialValueFromDouble(lDoubleValue) != BDX_SV_UNK) {
	  lGeneric = 1;
	} else {
	  lData->data.raw_data[i].double_value = lDoubleValue;
	}
	break;
      case EXTPTRSXP:
	/*
	 * according to BDR (mail on r-devel, 05-10-24) an EXTPTRSXP is not
	 * a vector, therefore LENGTH() does not work
	 */
	lTotalSize = 1; 
#if 0
	/* no EXTPTRSXP for arrays */
	if(lTotalSize != 1) {
	  bdx_free(lData);
	  BDX_TRACE(printf("SEXP2BDX[1]: EXTPTRSXP in array with %d elements\n",
			      lTotalSize));
	  return -7;
	}
#endif
	{
	  /* is it a COM object? */
	  RCOM_OBJHANDLE lHandle = com_getHandle(sexp);
	  LPSTREAM lStream = NULL;
	  int lRc = EXTPTRSXP2LPSTREAM(lHandle,&lStream); /* 0 for success */
	  
	  /* COM object is marshalled into stream
	   *
	   * 1. stream object holds reference to IDispatch
	   * 2. reference count is increased in the meanwhile
	   * 3. must use CoGetInterfaceAndReleaseStream() to unmarshal
	   * 4. Release() must be called on object afterwards
	   */
	  if (lRc == 0) {
	    /* we don't support generic pointers */
	    lData->data.raw_data[0].ptr = lStream;
	    lData->type |= BDX_HANDLE;
#if 0
	    if(lStream) {
	      /* marshalled COM object */
	    } else {
	      /* pointer */
	      lData->type |= BDX_POINTER;
	      lData->data.raw_data[0].ptr =
		(unsigned long) R_ExternalPtrAddr(sexp);
	    }
#endif
	  } else {
	    lData->type |= BDX_POINTER;
	    lData->data.raw_data[0].ptr = NULL; /* NULL pointer */
	    BDX_TRACE(printf("SEXP2BDX: error %d marshalling COM object at index %d\n",
				lRc,i));
	    
	  }
	  BDX_TRACE(printf("SEXP2BDX: base=%08x, type=%08x, ptr=%08x\n",
			   lData,lData->type,lData->data.raw_data[0].ptr));
	}
	break;
      case STRSXP:
	lData->data.raw_data[i].string_value = strdup(CHAR(STRING_ELT(sexp,i)));
	if(lData->data.raw_data[i].string_value == NULL) {
	  lData->data.raw_data[i].string_value = strdup("");
	}
	
	break;
      }
    }
  }

  /* VECSXP or REALSXP with special values */
  if(lGeneric) {
    if(lData->data.raw_data) {
      /* memory has been allocated */
      free(lData->data.raw_data);
    }
    if((lData->type & BDX_CMASK) == BDX_SCALAR) {
      /* scalar BDX_SPECIAL */
      lData->type = (lData->type & ~BDX_SMASK) | BDX_SPECIAL;
      lData->data.raw_data[0].special_value =
	getSpecialValueFromDouble(REAL(sexp)[0]);
    } else {
      /* BDX_GENERIC */
      lData->type = (lData->type & ~BDX_SMASK) | BDX_GENERIC;
      lData->data.raw_data_with_type =
	(BDX_RawDataWithType*) calloc(lTotalSize,
				      sizeof(BDX_RawDataWithType));
      switch(TYPEOF(sexp)) {
      case LGLSXP:
	for(i = 0;i < lTotalSize;i++) {
	  lData->data.raw_data_with_type[i].type = BDX_SPECIAL;
	  lData->data.raw_data_with_type[i].raw_data.special_value =
	    getSpecialValueFromLogical(LOGICAL(sexp)[i]);
	  if(lData->data.raw_data_with_type[i].raw_data.special_value ==
	     BDX_SV_UNK) {
	    lData->data.raw_data_with_type[i].type = BDX_BOOL;
	    lData->data.raw_data_with_type[i].raw_data.double_value =
	      LOGICAL(sexp)[i];
	  }
	}
	break;
      case INTSXP:
	for(i = 0;i < lTotalSize;i++) {
	  lData->data.raw_data_with_type[i].type = BDX_SPECIAL;
	  lData->data.raw_data_with_type[i].raw_data.special_value =
	    getSpecialValueFromInteger(INTEGER(sexp)[i]);
	  if(lData->data.raw_data_with_type[i].raw_data.special_value ==
	     BDX_SV_UNK) {
	    lData->data.raw_data_with_type[i].type = BDX_INT;
	    lData->data.raw_data_with_type[i].raw_data.double_value =
	      LOGICAL(sexp)[i];
	  }
	}
	break;
      case REALSXP:
	for(i = 0;i < lTotalSize;i++) {
	  lData->data.raw_data_with_type[i].type = BDX_SPECIAL;
	  lData->data.raw_data_with_type[i].raw_data.special_value =
	    getSpecialValueFromDouble(REAL(sexp)[i]);
	  if(lData->data.raw_data_with_type[i].raw_data.special_value ==
	     BDX_SV_UNK) {
	    lData->data.raw_data_with_type[i].type = BDX_DOUBLE;
	    lData->data.raw_data_with_type[i].raw_data.double_value =
	      REAL(sexp)[i];
	  }
	}
	break;
      default:
	for(i = 0;i < lTotalSize;i++) {
	  SEXP lElementSexp = VECTOR_ELT(sexp,i);
	  switch(TYPEOF(lElementSexp)) {
	  case NILSXP:
	    lData->data.raw_data_with_type[i].type = BDX_SPECIAL;
	    lData->data.raw_data_with_type[i].raw_data.special_value = BDX_SV_NULL;
	    break;
	  case LGLSXP:
	    lData->data.raw_data_with_type[i].type = BDX_SPECIAL;
	    lData->data.raw_data_with_type[i].raw_data.special_value =
	      getSpecialValueFromLogical(LOGICAL(lElementSexp)[0]);
	    if(lData->data.raw_data_with_type[i].raw_data.special_value == BDX_SV_UNK) {
	      lData->data.raw_data_with_type[i].type = BDX_BOOL;
	      lData->data.raw_data_with_type[i].raw_data.bool_value =
		LOGICAL(lElementSexp)[0];
	    }
	    break;
	  case INTSXP:
	    lData->data.raw_data_with_type[i].type = BDX_SPECIAL;
	    lData->data.raw_data_with_type[i].raw_data.special_value =
	      getSpecialValueFromInteger(INTEGER(lElementSexp)[0]);
	    if(lData->data.raw_data_with_type[i].raw_data.special_value == BDX_SV_UNK) {
	      lData->data.raw_data_with_type[i].type = BDX_INT;
	      lData->data.raw_data_with_type[i].raw_data.int_value =
		INTEGER(lElementSexp)[0];
	    }
	    break;
	  case REALSXP:
	    lData->data.raw_data_with_type[i].type = BDX_SPECIAL;
	    lData->data.raw_data_with_type[i].raw_data.special_value =
	      getSpecialValueFromDouble(REAL(lElementSexp)[0]);
	    if(lData->data.raw_data_with_type[i].raw_data.special_value == BDX_SV_UNK) {
	      lData->data.raw_data_with_type[i].type = BDX_DOUBLE;
	      lData->data.raw_data_with_type[i].raw_data.double_value =
		REAL(lElementSexp)[0];
	    }
	    break;
	  case EXTPTRSXP:
	    lData->data.raw_data_with_type[i].type = BDX_HANDLE;
	    /*
	     * according to BDR (mail on r-devel, 05-10-24) an EXTPTRSXP is not
	     * a vector, therefore LENGTH() does not work
	     */
	    lTotalSize = 1; 
	    /* no EXTPTRSXP for arrays */
	    {
	      /* is it a COM object? */
	      RCOM_OBJHANDLE lHandle = com_getHandle(lElementSexp);
	      LPSTREAM lStream = NULL;
	      int lRc = EXTPTRSXP2LPSTREAM(lHandle,&lStream); /* 0 for success */
	      /* COM object is marshalled into stream
	       *
	       * 1. stream object holds reference to IDispatch
	       * 2. reference count is increased in the meanwhile
	       * 3. must use CoGetInterfaceAndReleaseStream() to unmarshal
	       * 4. Release() must be called on object afterwards
	       */
	      if (lRc == 0) {
		/* we don't support generic pointers */
		lData->data.raw_data_with_type[i].raw_data.ptr = lStream;
		lData->type |= BDX_HANDLE;
	      } else {
		lData->type |= BDX_POINTER;
		lData->data.raw_data_with_type[i].raw_data.ptr =
		  NULL; /* NULL pointer */
		BDX_TRACE(printf("SEXP2BDX: error %d marshalling COM object at index %d\n",
				 lRc,i));
	      }
	    }
	    break;
	  case STRSXP:
	    lData->data.raw_data_with_type[i].type = BDX_STRING;
	    lData->data.raw_data_with_type[i].raw_data.string_value =
	      strdup(CHAR(STRING_ELT(lElementSexp,0)));
	    break;
	  default:
	    bdx_free(lData);
	    BDX_TRACE(printf("SEXP2BDX: unsupported SEXP type %d in VECSXP element %d\n",
			     TYPEOF(lElementSexp),i));
	    return -6;
	  }
	}
      }
    }
  }
  *ppBDXData = lData;

  return 0;
}

unsigned long bdx_get_datamode()
{
  return s_data_mode;
}
void bdx_set_datamode(unsigned long pDM)
{
  BDX_TRACE(printf("BDX: data mode set to %08x\n",pDM));
  s_data_mode = pDM;
}

static int EXTPTRSXP2LPSTREAM(RCOM_OBJHANDLE pHandle,
			      LPSTREAM* pStream)
{
  IUnknown* lUnk = NULL;
  LPSTREAM lStream = NULL;
  HRESULT hr = E_FAIL;

  if (pHandle == RCOM_NULLHANDLE) {
    return -1;
  }

  /* valid COM object */
  lUnk = (LPUNKNOWN) com_getObject(pHandle);

  /*
   * Marshal interface into stream (can be unmarshalled in any thread of
   * the same process. This may slow down things a bit, but we're on the
   * safe side if the BDX_Data is processed in a thread different from
   * the current one.
   */
  hr = CoMarshalInterThreadInterfaceInStream(&IID_IUnknown,
					     lUnk,
					     &lStream);
  if(FAILED(hr)) {
    BDX_TRACE(printf("SEXP2BDX: error %08x marshalling interface into stream\n",
		     hr));
    return -5;
  } else {
    *pStream = lStream;
  }
  return 0;
}

/* 06-06-24 | baier | NaN also transforms to IEEE double NaN for dm=1 */
static unsigned long getSpecialValueFromDouble(double pSEXPVal)
{
  if(ISNA(pSEXPVal)) {
    return BDX_SV_NA;
  } else if(pSEXPVal == R_PosInf) {
    if(bdx_get_datamode() == BDX_DM_NO_SPECIAL) {
      return BDX_SV_UNK;
    }
    return BDX_SV_INF;
  } else if(ISNAN(pSEXPVal)) {
    if(bdx_get_datamode() == BDX_DM_NO_SPECIAL) {
      return BDX_SV_UNK;
    }
    return BDX_SV_NAN;
  } else if(pSEXPVal == R_NegInf) {
    if(bdx_get_datamode() == BDX_DM_NO_SPECIAL) {
      return BDX_SV_UNK;
    }
    return BDX_SV_NINF;
  }
  return BDX_SV_UNK;
}
static unsigned long getSpecialValueFromInteger(int pSEXPVal)
{
  if(pSEXPVal == R_NaInt) {
    return BDX_SV_NA;
  }
  return BDX_SV_UNK;
}
static double getDoubleFromSpecialValue(unsigned long pSpecialValue)
{
  switch(pSpecialValue) {
  case BDX_SV_NULL:
  case BDX_SV_NA:
    /* NULL and NA both transform to NA */
    return R_NaReal;
  case BDX_SV_DIV0:
  case BDX_SV_INF:
    /* DIV/0 and +Inf transform to +Inf */
    return R_PosInf;
  case BDX_SV_NAN:
    /* NAN is just NaN */
    return R_NaN;
  case BDX_SV_NINF:
    /* -Inf is -Inf */
    return R_NegInf;
  case BDX_SV_UNK:
  default:
    return R_NaReal;
  }
}
