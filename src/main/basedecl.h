void Rsockconnect();
void Rsockopen();
void Rsocklisten();
void Rsockclose();
void Rsockread();
void Rsockwrite();
void call_dqagi();
void call_dqags();
void La_svd();
void La_rs();
void La_rg();
void La_zgesv();
void La_zgeqp3();
void qr_coef_cmplx();
void qr_qy_cmplx();
void La_svd_cmplx();
void La_rs_cmplx();
void La_rg_cmplx();
void La_chol();
void La_chol2inv();
void signrank_free();
void wilcox_free();
void band_ucv_bin();
void band_bcv_bin();
void band_phi4_bin();
void band_phi6_bin();
void band_den_bin();
void R_getTaskCallbackNames();
void R_removeTaskCallback();
void R_addTaskCallback();
void R_getSymbolInfo();
void R_isMethodsDispatchOn();
void R_traceOnOff();

SEXP Rrowsum_matrix(SEXP, SEXP, SEXP, SEXP);
SEXP Rrowsum_df(SEXP, SEXP, SEXP, SEXP);
SEXP do_mapply(SEXP, SEXP, SEXP, SEXP);

SEXP R_r2dtable(SEXP, SEXP, SEXP);

void F77_SYMBOL(dchdc)();
void F77_SYMBOL(dpbfa)();
void F77_SYMBOL(dpbsl)();

void Rf_InitGraphics();
void Rf_InitColors();
