/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2002-2012	The R Core Team.
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
 *  http://www.r-project.org/Licenses/
 *
 */
/*
 *  This file replaces the previously used ROUTINES file and is used to
 *  explicitly register native routines that are located in the R
 *  executable (e.g. R.bin, Rgui.exe) but which are intended to be
 *  accessible to S code via .C(), .Fortran(), .Call(), .External().
 *  The approach we use here is the regular registration mechanism that
 *  packages can use to explicitly list the symbols to be exported.
 *  For .C() and .Call() routines, we give the number of arguments
 *  expected.
 *  For .C() routines, we also specify the types of the arguments.
 *  For .Fortran() and .External() routines, we specify only the name
 *  and symbol.

 *  To add an entry, first determine by which interface the routine will
 *  be accessed:
 *   .C, .Call, .External or .Fortran
 *  Then add an entry to
 *    cMethods, callMethods, externalMethods, or fortranMethods
 *  respectively
 *
 *  DTL 14-Dec-2002
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Applic.h>
#include <R_ext/Linpack.h>


/*  These get the declarations of some routines refernced here but
    not explicitly declared.    This is necessary when we link with
    a C++ compiler because the linkage changes as the declarations
    are (currently) within extern "C" blocks.
*/
#include <R_ext/Callbacks.h>
#include <Rdynpriv.h>

#include "basedecl.h"

/* FIXME: bincode is no longer used in R, but is still used by
   packages misc3d mixOmics spam

   bincount is no longer used in R, used in pegas.
*/

static R_NativePrimitiveArgType bakslv_t[] = {REALSXP, INTSXP, INTSXP, REALSXP, INTSXP, INTSXP, REALSXP, INTSXP, INTSXP};

static R_NativePrimitiveArgType bincode_t[] = {REALSXP, INTSXP, REALSXP, INTSXP, INTSXP, LGLSXP, LGLSXP, LGLSXP};
static R_NativePrimitiveArgType bincount_t[] = {REALSXP, INTSXP, REALSXP, INTSXP, INTSXP, LGLSXP, LGLSXP, LGLSXP};

static R_NativePrimitiveArgType R_cumsum_t[] = {REALSXP, INTSXP, REALSXP, REALSXP};

static R_NativePrimitiveArgType find_interv_vec_t[] = {REALSXP, INTSXP, REALSXP, INTSXP, LGLSXP, LGLSXP, INTSXP};


static R_NativePrimitiveArgType R_max_col_t[] = {REALSXP, INTSXP, INTSXP, INTSXP, INTSXP};

static R_NativePrimitiveArgType R_pretty_t[] = {REALSXP, REALSXP, INTSXP, INTSXP, REALSXP, REALSXP, INTSXP};
static R_NativePrimitiveArgType R_rowsum_t[] = {INTSXP, REALSXP, REALSXP, REALSXP};

static R_NativePrimitiveArgType stemleaf_t[] = {REALSXP, INTSXP, REALSXP, INTSXP, REALSXP};

/* Note the ANYSXP in the first place.
   Doesn't quite work.  Needs investigation.
   See the odd first argument in the routine's definition.

static R_NativePrimitiveArgType str_signif_t[] = {ANYSXP, INTSXP, STRSXP, INTSXP, INTSXP, STRSXP, STRSXP, STRSXP};
*/

static R_NativePrimitiveArgType R_tabulate_t[] = {INTSXP, INTSXP, INTSXP, INTSXP};

static R_NativePrimitiveArgType Rsockconnect_t[] = {INTSXP, STRSXP};
static R_NativePrimitiveArgType Rsockopen_t[] = {INTSXP};
static R_NativePrimitiveArgType Rsocklisten_t[] = {INTSXP, STRSXP, INTSXP};
static R_NativePrimitiveArgType Rsockclose_t[] = {INTSXP};
static R_NativePrimitiveArgType Rsockread_t[] = {INTSXP, STRSXP, INTSXP};
static R_NativePrimitiveArgType Rsockwrite_t[] = {INTSXP, STRSXP, INTSXP, INTSXP, INTSXP};


#ifdef PROBLEMS
static R_NativePrimitiveArgType fft_factor_t[] = {REALSXP, INTSXP, REALSXP, INTSXP, INTSXP, INTSXP, INTSXP};

static R_NativePrimitiveArgType fft_work_t[] = {REALSXP, REALSXP, };
static R_NativePrimitiveArgType fdhess_t[] = {};
#endif

#define CDEF(name)  {#name, (DL_FUNC) &name, sizeof(name ## _t)/sizeof(name ## _t[0]), name ##_t}

static R_CMethodDef cMethods [] = {
    CDEF(bakslv),
    CDEF(bincode),
    CDEF(bincount),
    CDEF(R_cumsum),
    CDEF(find_interv_vec),
    CDEF(R_max_col),
    CDEF(R_pretty),
    /* this is called by Hmisc, although no longer used in R */
    CDEF(R_rowsum),
    CDEF(stemleaf),
#if 0
    CDEF(str_signif),
#else
    {"str_signif", (DL_FUNC) &str_signif, 8, NULL},
#endif
    CDEF(R_tabulate),

    /* Sockets */
    CDEF(Rsockconnect),
    CDEF(Rsockopen),
    CDEF(Rsocklisten),
    CDEF(Rsockclose),
    CDEF(Rsockread),
    CDEF(Rsockwrite),

    /* nmath cleanup */
    {"signrank_free", (DL_FUNC)&signrank_free, 0, NULL},
    {"wilcox_free", (DL_FUNC)&wilcox_free, 0, NULL},
    {NULL, NULL, 0}
};


#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}


static R_CallMethodDef callMethods [] = {
    /* lapack */
    CALLDEF(La_svd, 7),
    CALLDEF(La_rs, 2),
    CALLDEF(La_rg, 2),
    CALLDEF(La_dlange, 2),
    CALLDEF(La_dgecon, 2),
    CALLDEF(La_dtrcon, 2),
    CALLDEF(La_zgecon, 2),
    CALLDEF(La_ztrcon, 2),
    CALLDEF(La_zgesv, 2),
    CALLDEF(La_zgeqp3, 1),
    CALLDEF(qr_coef_cmplx, 2),
    CALLDEF(qr_qy_cmplx, 3),
    CALLDEF(La_svd_cmplx, 6),
    CALLDEF(La_rs_cmplx, 2),
    CALLDEF(La_rg_cmplx, 2),
    CALLDEF(La_chol2inv, 2),
    CALLDEF(La_chol, 2),
    CALLDEF(La_dgesv, 3),
    CALLDEF(La_dgeqp3, 1),
    CALLDEF(qr_coef_real, 2),
    CALLDEF(qr_qy_real, 3),
    CALLDEF(det_ge_real, 2),

    /* In ../main/unique.c to use hashing. */
    CALLDEF(Rrowsum_matrix, 5),
    CALLDEF(Rrowsum_df, 5),

    /* Top-level task callbacks */
    CALLDEF(R_getTaskCallbackNames, 0),
    CALLDEF(R_removeTaskCallback, 1),
    CALLDEF(R_addTaskCallback, 4),

    /* Reflectance for the dynamically loaded native symbols. */
    CALLDEF(R_getSymbolInfo, 3),
    CALLDEF(R_getDllTable, 0),
    CALLDEF(R_getRegisteredRoutines, 1),

    /* mapply */
    CALLDEF(do_mapply, 4),

    /* in ../main/random.c to generate 'sequences' of random 2-d tables
     * using Patefield's algorithm.
     */
    CALLDEF(R_r2dtable, 3),
    CALLDEF(R_shortRowNames, 2),
    CALLDEF(R_copyDFattr, 2),

    /* Methods related routines. */
    CALLDEF(R_isMethodsDispatchOn, 1),
    CALLDEF(R_traceOnOff, 1),
    CALLDEF(R_isS4Object, 1),
    CALLDEF(R_setS4Object, 3),
    CALLDEF(R_do_new_object, 1),
    CALLDEF(R_get_primname, 1),

    /* compression and serialization routines */
    CALLDEF(R_compress1, 1),
    CALLDEF(R_decompress1, 1),
    CALLDEF(R_serializeb, 5),
    CALLDEF(R_serialize, 5),
    CALLDEF(R_unserialize, 2),
    CALLDEF(R_setFileTime, 2),

    /* lazy loading support */
    CALLDEF(R_getVarsFromFrame, 3),
    CALLDEF(R_lazyLoadDBinsertValue, 5),
    CALLDEF(R_lazyLoadDBflush, 1),

#ifdef BC_PROFILING
    // These have no interface in R, so used directly by .Call
    CALLDEF(R_getbcprofcounts, 0),
    CALLDEF(R_startbcprof, 0),
    CALLDEF(R_stopbcprof, 0),
#endif

    /* base graphics */
    CALLDEF(Rg_contourDef, 0),

    CALLDEF(bitwiseNot, 1),
    CALLDEF(bitwiseAnd, 2),
    CALLDEF(bitwiseOr,  2),
    CALLDEF(bitwiseXor, 2),

    CALLDEF(crc64ToString, 1),
    CALLDEF(BinCode, 4),
    CALLDEF(BinCount, 4),
    CALLDEF(R_Tabulate, 2),
    CALLDEF(BinCount, 4),
    CALLDEF(FindIntervVec, 4),

    {NULL, NULL, 0}
};


#define EXTDEF(name, n)  {#name, (DL_FUNC) &name, n}

static R_ExternalMethodDef externalMethods [] = {
    EXTDEF(call_dqags, 7),
    EXTDEF(call_dqagi, 7),
    {NULL, NULL, 0}
};


#define FDEF(name)  {#name, (DL_FUNC) &F77_SYMBOL(name), -1, NULL}
static R_FortranMethodDef fortranMethods[] = {
    FDEF(ch2inv),
    FDEF(chol),
    FDEF(cg),
    FDEF(ch),
    FDEF(rg),
    FDEF(rs),
    /* Linpack */
    FDEF(dchdc),
    FDEF(dpbfa),
    FDEF(dpbsl),
    FDEF(dpoco),
    FDEF(dpodi),
    FDEF(dpofa),
    FDEF(dposl),
    FDEF(dqrcf),
    FDEF(dqrdc),
    FDEF(dqrdc2),
    FDEF(dqrls),
    FDEF(dqrqty),
    FDEF(dqrqy),
    FDEF(dqrrsd),
    FDEF(dqrsl),
    FDEF(dqrxb),
    FDEF(dsvdc),
    FDEF(dtrsl),
    FDEF(dtrco),
    {NULL, NULL, 0}
};


void attribute_hidden
R_init_base(DllInfo *dll)
{
    R_registerRoutines(dll, cMethods, callMethods,
		       fortranMethods, externalMethods);
    R_useDynamicSymbols(dll, FALSE);
}
