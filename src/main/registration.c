#include "R.h"
#include "Rdefines.h"
#include "R_ext/Rdynload.h"

#include "Defn.h"
#include "R_ext/Applic.h"
#include "R_ext/Rlapack.h"
#include "basedecl.h"

/*
  Omitted (relative to those in ROUTINES)
fft_factor
fft_work
fdhess
optif9
*/

R_NativePrimitiveArgType R_approx_t[] = {REALSXP, REALSXP, INTSXP, REALSXP, INTSXP, INTSXP, REALSXP, REALSXP, REALSXP};
R_NativePrimitiveArgType bakslv_t[] = {REALSXP, INTSXP, INTSXP, REALSXP, INTSXP, INTSXP, REALSXP, INTSXP, INTSXP};

R_NativePrimitiveArgType bincode_t[] = {REALSXP, INTSXP, REALSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP};
R_NativePrimitiveArgType bincount_t[] = {REALSXP, INTSXP, REALSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP};

R_NativePrimitiveArgType R_chull_t[] = {INTSXP, REALSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP};

R_NativePrimitiveArgType R_cumsum_t[] = {REALSXP, INTSXP, REALSXP, REALSXP};

R_NativePrimitiveArgType find_interv_vec_t[] = {REALSXP, INTSXP, REALSXP, INTSXP, INTSXP, LGLSXP, INTSXP};

R_NativePrimitiveArgType loglin_t[] = {INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, 
                                     REALSXP, REALSXP, INTSXP, INTSXP, REALSXP,
                                     INTSXP, REALSXP, REALSXP, INTSXP, REALSXP, 
				     INTSXP, INTSXP};

R_NativePrimitiveArgType lowess_t[] = {REALSXP, REALSXP, INTSXP, REALSXP, 
				       INTSXP, REALSXP, REALSXP, REALSXP, REALSXP};


R_NativePrimitiveArgType massdist_t[] = {REALSXP, INTSXP, REALSXP, REALSXP, REALSXP, INTSXP};

R_NativePrimitiveArgType R_max_col_t[] = {REALSXP, INTSXP, INTSXP, INTSXP};

R_NativePrimitiveArgType R_pretty_t[] = {REALSXP, REALSXP, INTSXP, INTSXP, REALSXP, REALSXP, INTSXP};
R_NativePrimitiveArgType R_rowsum_t[] = {INTSXP, REALSXP, REALSXP, REALSXP};

R_NativePrimitiveArgType spline_coef_t[] = {INTSXP, INTSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP};
R_NativePrimitiveArgType spline_eval_t[] = {INTSXP, INTSXP, REALSXP, REALSXP, INTSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP};

R_NativePrimitiveArgType stemleaf_t[] = {REALSXP, INTSXP, REALSXP, INTSXP, REALSXP};

/* Note the ANYSXP in the first place. Doesn't quite work. Needs investigation.
   See the odd first argument in the routine's definition.
*/
R_NativePrimitiveArgType str_signif_t[] = {ANYSXP, INTSXP, STRSXP, INTSXP, INTSXP, STRSXP, STRSXP, STRSXP};

R_NativePrimitiveArgType R_tabulate_t[] = {INTSXP, INTSXP, INTSXP, INTSXP};

R_NativePrimitiveArgType Rsockconnect_t[] = {INTSXP, STRSXP};
R_NativePrimitiveArgType Rsockopen_t[] = {INTSXP};
R_NativePrimitiveArgType Rsocklisten_t[] = {INTSXP, STRSXP, INTSXP};
R_NativePrimitiveArgType Rsockclose_t[] = {INTSXP};
R_NativePrimitiveArgType Rsockread_t[] = {INTSXP, STRSXP, INTSXP};
R_NativePrimitiveArgType Rsockwrite_t[] = {INTSXP, STRSXP, INTSXP, INTSXP, INTSXP};

R_NativePrimitiveArgType band_ucv_bin_t[] = {INTSXP, INTSXP, REALSXP, INTSXP, REALSXP, REALSXP};
R_NativePrimitiveArgType band_bcv_bin_t[] = {INTSXP, INTSXP, REALSXP, INTSXP, REALSXP, REALSXP};
R_NativePrimitiveArgType band_phi4_bin_t[] = {INTSXP, INTSXP, REALSXP, INTSXP, REALSXP, REALSXP};
R_NativePrimitiveArgType band_phi6_bin_t[] = {INTSXP, INTSXP, REALSXP, INTSXP, REALSXP, REALSXP};
R_NativePrimitiveArgType band_den_bin_t[] = {INTSXP, INTSXP, REALSXP, REALSXP, INTSXP};


#ifdef PROBLEMS
R_NativePrimitiveArgType fft_factor_t[] = {REALSXP, INTSXP, REALSXP, INTSXP, INTSXP, INTSXP, INTSXP};

R_NativePrimitiveArgType fft_work_t[] = {REALSXP, REALSXP, };
R_NativePrimitiveArgType fdhess_t[] = {};
#endif

#define CDEF(name)  {#name, (DL_FUNC) &name, sizeof(name ## _t)/sizeof(name ## _t[0]), name ##_t}

static R_CMethodDef cmethods [] = {
    CDEF(R_approx),
    CDEF(bakslv),
    CDEF(bincode),
    CDEF(bincount),
    CDEF(R_chull),
    CDEF(R_cumsum),

    CDEF(find_interv_vec),
    CDEF(loglin),
    CDEF(lowess),
    CDEF(massdist),
    CDEF(R_max_col),
    CDEF(R_pretty),
    CDEF(R_rowsum),

    CDEF(spline_coef),
    CDEF(spline_eval),
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

/* bandwidth selectors */
    CDEF(band_ucv_bin),
    CDEF(band_bcv_bin),
    CDEF(band_phi4_bin),
    CDEF(band_phi6_bin),
    CDEF(band_den_bin),

    {"InitGraphics", (DL_FUNC)&Rf_InitGraphics, 0, NULL},    
    {"InitColors", (DL_FUNC)&Rf_InitColors, 0, NULL},
    {NULL, NULL, 0}
};


#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static R_CallMethodDef callMethods [] = {
/* lapack */
    CALLDEF(La_svd, 7),
    CALLDEF(La_rs, 3),
    CALLDEF(La_rg, 2),
    CALLDEF(La_zgesv, 2),
    CALLDEF(La_zgeqp3, 1),
    CALLDEF(qr_coef_cmplx, 2),
    CALLDEF(qr_qy_cmplx, 3),
    CALLDEF(La_svd_cmplx, 6),
    CALLDEF(La_rs_cmplx, 2),
    CALLDEF(La_rg_cmplx, 2),
    CALLDEF(La_chol2inv, 2),
    CALLDEF(La_chol, 1),

/* in ../main/unique.c to use hashing */
    CALLDEF(Rrowsum_matrix, 4),
    CALLDEF(Rrowsum_df, 4),

/* Top-level task callbacks */ 
    CALLDEF(R_getTaskCallbackNames, 0),
    CALLDEF(R_removeTaskCallback, 1),
    CALLDEF(R_addTaskCallback, 4),

/* Reflectance for the dynamically loaded native symbols */
    CALLDEF(R_getSymbolInfo, 2),

/* Methods related routines. */
    CALLDEF(R_isMethodsDispatchOn, 1),
    CALLDEF(R_traceOnOff, 1),
    {NULL, NULL, 0}
};


#define EXTDEF(name)  {#name, (DL_FUNC) &name}

static R_ExternalMethodDef externalMethods [] = {
/* integrate */
    EXTDEF(call_dqags),
    EXTDEF(call_dqagi),
    {NULL, NULL, 0}
};


#define FDEF(name)  {#name, (DL_FUNC) &F77_SYMBOL(name), -1, NULL}
R_FortranMethodDef fortranMethods[] = {
/* Linpack */
    FDEF(ch2inv),
    FDEF(chol),
    FDEF(cg),
    FDEF(ch),
    FDEF(rg),
    FDEF(rs),
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
    FDEF(dqrsl),
    FDEF(dqrqty),
    FDEF(dqrqy),
    FDEF(dqrrsd),
    FDEF(dqrsl),
    FDEF(dqrxb),
    FDEF(dsvdc),
    FDEF(dtrsl),
    FDEF(dtrco),
    FDEF(lminfl),
    {NULL, NULL, 0}
};


void
R_init_base(DllInfo *dll)
{
    R_registerRoutines(dll, cmethods, callMethods,
		       fortranMethods, externalMethods);
}

