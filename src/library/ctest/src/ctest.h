#ifndef R_CTEST_H
#define R_CTEST_H

#include <R.h>

void chisqsim(int *nrow, int *ncol, int *nrowt, int *ncolt, int *n,
	 int *b, double *expected, int *observed, double *fact,
         int *jwork, double *results);
void d2x2xk(Sint *k, double *m, double *n, double *t, double *d);
void fexact(Sint *nrow, Sint *ncol, double *table, Sint *ldtabl,
       double *expect, double *percnt, double *emin, double *prt,
	    double *pre, Sint *workspace);
void kendall_tau(Sint *n, double *x, double *y, double *tau);
void pansari(Sint *len, double *x, Sint *m, Sint *n);
void pkendall(Sint *len, double *x, Sint *n);
void pkstwo(Sint *n, double *x, double *tol) ;
void prho(int *n, int *is, double *pv, int *ifault, int *lower_tail);
void psmirnov2x(double *x, Sint *m, Sint *n);
void qansari(Sint *len, double *x, Sint *m, Sint *n);
void swilk(int *init, float *x, int *n, int *n1, int *n2,
            float *a,  double *w, double *pw, int *ifault);


#endif

