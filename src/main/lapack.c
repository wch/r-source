/*
 *  R : A Computer Language for Statistical Data Analysis
  *  Copyright (C) 2001 The R Development Core Team
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
#include <config.h>
#endif

#include <Defn.h>
#include "R_ext/Rdynpriv.h"

typedef SEXP  (*sDL_FUNC)();
static sDL_FUNC ptr_svd, ptr_rs, ptr_rg, ptr_zgesv, ptr_zgeqp3,
    ptr_qr_coef_cmplx, ptr_qr_qy_cmplx, ptr_svd_cmplx, 
    ptr_rs_cmplx, ptr_rg_cmplx;

/*
SEXP La_svd(SEXP jobu, SEXP jobv, SEXP x, SEXP s, SEXP u, SEXP v)
SEXP La_rs(SEXP x, SEXP only_values)
SEXP La_rg(SEXP x, SEXP only_values)
SEXP La_zgesv(SEXP A, SEXP B)
SEXP La_zgeqp3(SEXP A)
SEXP qr_coef_cmplx(SEXP Q, SEXP B)
SEXP qr_qy_cmplx(SEXP Q, SEXP B, SEXP trans)
SEXP La_svd_cmplx(SEXP jobu, SEXP jobv, SEXP x, SEXP s, SEXP u, SEXP v)
SEXP La_rs_complex(SEXP x, SEXP only_values)
SEXP La_rg_complex(SEXP x, SEXP only_values)
*/

static int initialized = 0;

static void La_Init(void)
{
    int res = moduleCdynload("lapack", 1, 1);
    initialized = -1;
    if(!res) return;
    
    if(!(ptr_svd = (sDL_FUNC)R_FindSymbol("modLa_svd", "lapack", NULL))) return;
    if(!(ptr_rs = (sDL_FUNC)R_FindSymbol("modLa_rs", "lapack", NULL))) return;
    if(!(ptr_rg = (sDL_FUNC)R_FindSymbol("modLa_rg", "lapack", NULL))) return;
    if(!(ptr_zgesv = (sDL_FUNC)R_FindSymbol("modLa_zgesv", "lapack", NULL))) return;
    if(!(ptr_zgeqp3 = (sDL_FUNC)R_FindSymbol("modLa_zgeqp3", "lapack", NULL))) return;
    if(!(ptr_qr_coef_cmplx = 
	 (sDL_FUNC)R_FindSymbol("modqr_coef_cmplx", "lapack", NULL))) return;
    if(!(ptr_qr_qy_cmplx = 
    (sDL_FUNC)R_FindSymbol("modqr_qy_cmplx", "lapack", NULL))) return;
    if(!(ptr_svd_cmplx =  
	 (sDL_FUNC)R_FindSymbol("modLa_svd_cmplx", "lapack", NULL))) return;
    if(!(ptr_rs_cmplx = 
	 (sDL_FUNC)R_FindSymbol("modLa_rs_cmplx", "lapack", NULL))) return;
    if(!(ptr_rg_cmplx = 
	 (sDL_FUNC)R_FindSymbol("modLa_rg_cmplx", "lapack", NULL))) return;

    initialized = 1;    
    return;
}

SEXP La_svd(SEXP jobu, SEXP jobv, SEXP x, SEXP s, SEXP u, SEXP v)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr_svd)(jobu, jobv, x, s, u, v);
    else {
	error("lapack routines cannot be loaded");
	return R_NilValue;
    }
}

SEXP La_svd_cmplx(SEXP jobu, SEXP jobv, SEXP x, SEXP s, SEXP u, SEXP v)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr_svd_cmplx)(jobu, jobv, x, s, u, v);
    else {
	error("lapack routines cannot be loaded");
	return R_NilValue;
    }
}

SEXP La_rs(SEXP x, SEXP only_values)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr_rs)(x, only_values);
    else {
	error("lapack routines cannot be loaded");
	return R_NilValue;
    }
}

SEXP La_rs_cmplx(SEXP x, SEXP only_values)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr_rs_cmplx)(x, only_values);
    else {
	error("lapack routines cannot be loaded");
	return R_NilValue;
    }
}

SEXP La_rg(SEXP x, SEXP only_values)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr_rg)(x, only_values);
    else {
	error("lapack routines cannot be loaded");
	return R_NilValue;
    }
}

SEXP La_rg_cmplx(SEXP x, SEXP only_values)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr_rg_cmplx)(x, only_values);
    else {
	error("lapack routines cannot be loaded");
	return R_NilValue;
    }
}

SEXP La_zgesv(SEXP A, SEXP B)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr_zgesv)(A, B);
    else {
	error("lapack routines cannot be loaded");
	return R_NilValue;
    }
}

SEXP La_zgeqp3(SEXP A)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr_zgeqp3)(A);
    else {
	error("lapack routines cannot be loaded");
	return R_NilValue;
    }
}

SEXP qr_coef_cmplx(SEXP Q, SEXP B)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr_qr_coef_cmplx)(Q, B);
    else {
	error("lapack routines cannot be loaded");
	return R_NilValue;
    }
}

SEXP qr_qy_cmplx(SEXP Q, SEXP B, SEXP trans)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr_qr_qy_cmplx)(Q, B, trans);
    else {
	error("lapack routines cannot be loaded");
	return R_NilValue;
    }
}
