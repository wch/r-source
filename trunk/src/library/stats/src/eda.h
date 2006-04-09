/* ./line.c : */

void tukeyline(double *x, double *y, double *z, double *w, 
	       int *n, double *coef);

/* ./smooth.c : */

typedef enum { 
    sm_NO_ENDRULE, sm_COPY_ENDRULE, sm_TUKEY_ENDRULE 
} R_SM_ENDRULE;


/* Callable from R : */
void Rsm_3RSR (double *x, double *y, int *n, int *end_rule, int *iter);
void Rsm_3RSS (double *x, double *y, int *n, int *end_rule, int *iter);
void Rsm_3RS3R(double *x, double *y, int *n, int *end_rule, int *iter);
void Rsm_3R   (double *x, double *y, int *n, int *end_rule, int *iter);
void Rsm_3    (double *x, double *y, int *n, int *end_rule, int *changed);

void Rsm_S    (double *x, double *y, int *n, int *do_ends, int *changed);
