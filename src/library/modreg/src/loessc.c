/*
 * The authors of this software are Cleveland, Grosse, and Shyu.
 * Copyright (c) 1989, 1992 by AT&T.
 * Permission to use, copy, modify, and distribute this software for any
 * purpose without fee is hereby granted, provided that this entire notice
 * is included in all copies of any software which is or includes a copy
 * or modification of this software and in all copies of the supporting
 * documentation for such software.
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, NEITHER THE AUTHORS NOR AT&T MAKE ANY
 * REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY
 * OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/*
 *  Altered by B.D. Ripley to use  F77_SYMBOL, declare routines before use.
 */
#include <stdio.h>
#include <math.h>
#include <S.h>

#define F77_SUB(x)    F77_SYMBOL(x)

void loess_workspace();
void loess_prune();
void loess_grow ();
void loess_free();
void F77_SUB(lowesa)();
void F77_SUB(lowesb)();
void F77_SUB(lowesc)();
void F77_SUB(lowesd)();
void F77_SUB(lowese)();
void F77_SUB(lowesf)();
void F77_SUB(lowesl)();
void F77_SUB(ehg169)();
void F77_SUB(ehg196)();


static void
warnmsg(char *string)
{
  PROBLEM "%s\n", string WARNING(NULL_ENTRY);
}


#undef min
#undef max

#define	min(x,y)  ((x) < (y) ? (x) : (y))
#define	max(x,y)  ((x) > (y) ? (x) : (y))
#define	GAUSSIAN	1
#define SYMMETRIC	0

static long	*iv, liv, lv, tau;
static double	*v;

void
loess_raw(y, x, weights, robust, d, n, span, degree, nonparametric, 
	drop_square, sum_drop_sqr, cell, surf_stat, surface, parameter, a, 
	xi, vert, vval, diagonal, trL, one_delta, two_delta, setLf)
double	*y, *x, *weights, *robust, *span, *cell, *surface, *xi, *vert, 
	*vval, *diagonal, *trL, *one_delta, *two_delta;
long	*d, *n, *parameter, *a, *degree, *nonparametric, *drop_square, 
	*sum_drop_sqr, *setLf;
char	**surf_stat;
{
	long	zero = 0, one = 1, two = 2, nsing, i, k;
	double	*hat_matrix, *LL;

	*trL = 0;	
	
	loess_workspace(d, n, span, degree, nonparametric, drop_square, 
		sum_drop_sqr, setLf);
        v[1] = *cell;
	if(!strcmp(*surf_stat, "interpolate/none")) {
		F77_SUB(lowesb)(x, y, robust, &zero, &zero, iv, &liv, &lv, v);
		F77_SUB(lowese)(iv, &liv, &lv, v, n, x, surface);
		loess_prune(parameter, a, xi, vert, vval);
	}			
	else if (!strcmp(*surf_stat, "direct/none")) {
		F77_SUB(lowesf)(x, y, robust, iv, &liv, &lv, v, n, x,
			&zero, &zero, surface);
	}
	else if (!strcmp(*surf_stat, "interpolate/1.approx")) {
		F77_SUB(lowesb)(x, y, weights, diagonal, &one, iv, &liv, &lv, v);
		F77_SUB(lowese)(iv, &liv, &lv, v, n, x, surface);
		nsing = iv[29];
		for(i = 0; i < (*n); i++) *trL = *trL + diagonal[i];
		F77_SUB(lowesa)(trL, n, d, &tau, &nsing, one_delta, two_delta);
		loess_prune(parameter, a, xi, vert, vval);
	}
        else if (!strcmp(*surf_stat, "interpolate/2.approx")) {
		F77_SUB(lowesb)(x, y, robust, &zero, &zero, iv, &liv, &lv, v);
		F77_SUB(lowese)(iv, &liv, &lv, v, n, x, surface);
		nsing = iv[29];
		F77_SUB(ehg196)(&tau, d, span, trL);
		F77_SUB(lowesa)(trL, n, d, &tau, &nsing, one_delta, two_delta);
		loess_prune(parameter, a, xi, vert, vval);
	}
	else if (!strcmp(*surf_stat, "direct/approximate")) {
		F77_SUB(lowesf)(x, y, weights, iv, &liv, &lv, v, n, x,
			diagonal, &one, surface);
		nsing = iv[29];
		for(i = 0; i < (*n); i++) *trL = *trL + diagonal[i];
		F77_SUB(lowesa)(trL, n, d, &tau, &nsing, one_delta, two_delta);
	}
	else if (!strcmp(*surf_stat, "interpolate/exact")) {
		hat_matrix = Calloc((*n)*(*n), double);
		LL = Calloc((*n)*(*n), double);
		F77_SUB(lowesb)(x, y, weights, diagonal, &one, iv, &liv, &lv, v);
		F77_SUB(lowesl)(iv, &liv, &lv, v, n, x, hat_matrix);
		F77_SUB(lowesc)(n, hat_matrix, LL, trL, one_delta, two_delta);
		F77_SUB(lowese)(iv, &liv, &lv, v, n, x, surface);
		loess_prune(parameter, a, xi, vert, vval);
		Free(hat_matrix);
		Free(LL);
	}
	else if (!strcmp(*surf_stat, "direct/exact")) {
		hat_matrix = Calloc((*n)*(*n), double);
		LL = Calloc((*n)*(*n), double);
		F77_SUB(lowesf)(x, y, weights, iv, liv, lv, v, n, x,
			hat_matrix, &two, surface);
		F77_SUB(lowesc)(n, hat_matrix, LL, trL, one_delta, two_delta);
                k = (*n) + 1;
		for(i = 0; i < (*n); i++)
			diagonal[i] = hat_matrix[i * k];
		Free(hat_matrix);
		Free(LL);
	}
	loess_free();
}

void
loess_dfit(y, x, x_evaluate, weights, span, degree, nonparametric, 
	drop_square, sum_drop_sqr, d, n, m, fit)
double	*y, *x, *x_evaluate, *weights, *span, *fit;
long	*degree, *nonparametric, *drop_square, *sum_drop_sqr, *d, *n, *m; 
{
	long	zero = 0;
	
        loess_workspace(d, n, span, degree, nonparametric, drop_square,
                sum_drop_sqr, &zero);
	F77_SUB(lowesf)(x, y, weights, iv, &liv, &lv, v, m, x_evaluate,
			&zero, &zero, fit);
	loess_free();
}

void
loess_dfitse(y, x, x_evaluate, weights, robust, family, span, degree, 
	nonparametric, drop_square, sum_drop_sqr, d, n, m, fit, L)
double	*y, *x, *x_evaluate, *weights, *robust, *span, *fit, *L;
long	*family, *degree, *nonparametric, *drop_square, *sum_drop_sqr, 
	*d, *n, *m; 
{
	long	zero = 0, two = 2;
	
        loess_workspace(d, n, span, degree, nonparametric, drop_square,
                sum_drop_sqr, &zero);
	if(*family == GAUSSIAN)
		F77_SUB(lowesf)(x, y, weights, iv, &liv, &lv, v, m, 
				x_evaluate, L, &two, fit);
	else if(*family == SYMMETRIC)
	{
		F77_SUB(lowesf)(x, y, weights, iv, &liv, &lv, v, m,
				x_evaluate, L, &two, fit);
		F77_SUB(lowesf)(x, y, robust, iv, &liv, &lv, v, m,
				x_evaluate, &zero, &zero, fit);
	}	
	loess_free();
}
void
loess_ifit(parameter, a, xi, vert, vval, m, x_evaluate, fit)
double	*xi, *vert, *vval, *x_evaluate, *fit;
long	*parameter, *a, *m;
{
	loess_grow(parameter, a, xi, vert, vval);
	F77_SUB(lowese)(iv, &liv, &lv, v, m, x_evaluate, fit);
	loess_free();
}

void
loess_ise(y, x, x_evaluate, weights, span, degree, nonparametric, 
	drop_square, sum_drop_sqr, cell, d, n, m, fit, L)
double	*y, *x, *x_evaluate, *weights, *span, *cell, *fit, *L;
long	*degree, *nonparametric, *drop_square, *sum_drop_sqr, *d, *n, *m; 
{
	long	zero = 0, one = 1;
	
        loess_workspace(d, n, span, degree, nonparametric, drop_square,
                sum_drop_sqr, &one);
	v[1] = *cell;
	F77_SUB(lowesb)(x, y, weights, &zero, &zero, iv, &liv, &lv, v);
	F77_SUB(lowesl)(iv, &liv, &lv, v, m, x_evaluate, L);
	loess_free();
}

void
loess_workspace(d, n, span, degree, nonparametric, drop_square, 
	sum_drop_sqr, setLf)
long	*d, *n, *degree, *nonparametric, *drop_square, *sum_drop_sqr, 
	*setLf;
double	*span;
{
	long	D, N, tau0, nvmax, nf, version = 106, i;

	D = *d;
	N = *n;
	nvmax = max(200, N);
        nf = min(N, floor(N * (*span)));
        tau0 = ((*degree) > 1) ? ((D + 2) * (D + 1) * 0.5) : (D + 1);
        tau = tau0 - (*sum_drop_sqr);
        lv = 50 + (3 * D + 3) * nvmax + N + (tau0 + 2) * nf;
	liv = 50 + ((long)pow((double)2, (double)D) + 4) * nvmax + 2 * N;
	if(*setLf) {
		lv = lv + (D + 1) * nf * nvmax;
		liv = liv + nf * nvmax;	
	}
        iv = Calloc(liv, long);
        v = Calloc(lv, double);

        F77_SUB(lowesd)(&version, iv, &liv, &lv, v, d, n, span, degree, 
			&nvmax, setLf);
        iv[32] = *nonparametric;
        for(i = 0; i < D; i++)
                iv[i + 40] = drop_square[i];
}

void
loess_prune(parameter, a, xi, vert, vval)
double	*xi, *vert, *vval;
long	*parameter, *a;
{
	long	d, vc, a1, v1, xi1, vv1, nc, nv, nvmax, i, k;
	
	d = iv[1];
	vc = iv[3] - 1;
	nc = iv[4];
	nv = iv[5];
	a1 = iv[6] - 1;
	v1 = iv[10] - 1;
	xi1 = iv[11] - 1;
	vv1 = iv[12] - 1;
	nvmax = iv[13];

	for(i = 0; i < 5; i++)
		parameter[i] = iv[i + 1];
	parameter[5] = iv[21] - 1;
	parameter[6] = iv[14] - 1;

	for(i = 0; i < d; i++){
		k = nvmax * i;
		vert[i] = v[v1 + k];
		vert[i + d] = v[v1 + vc + k];
	}
	for(i = 0; i < nc; i++) {
		xi[i] = v[xi1 + i];
		a[i] = iv[a1 + i];
	}
	k = (d + 1) * nv;
	for(i = 0; i < k; i++)
		vval[i] = v[vv1 + i];
}

void
loess_grow(parameter, a, xi, vert, vval)
double	*xi, *vert, *vval;
long	*parameter, *a;
{
	long	d, vc, nc, nv, a1, v1, xi1, vv1, i, k;

	d = parameter[0];
	vc = parameter[2];
	nc = parameter[3];
	nv = parameter[4];
	liv = parameter[5];
	lv = parameter[6];
	iv = Calloc(liv, long);
	v = Calloc(lv, double);

	iv[1] = d;
	iv[2] = parameter[1];
	iv[3] = vc;
	iv[5] = iv[13] = nv;
	iv[4] = iv[16] = nc;
	iv[6] = 50;
	iv[7] = iv[6] + nc;
	iv[8] = iv[7] + vc * nc;
	iv[9] = iv[8] + nc;
	iv[10] = 50;
	iv[12] = iv[10] + nv * d;
	iv[11] = iv[12] + (d + 1) * nv;
	iv[27] = 173;

	v1 = iv[10] - 1;
	xi1 = iv[11] - 1;
	a1 = iv[6] - 1;
	vv1 = iv[12] - 1;
	
        for(i = 0; i < d; i++) {
		k = nv * i;
		v[v1 + k] = vert[i];
		v[v1 + vc - 1 + k] = vert[i + d];
	}
        for(i = 0; i < nc; i++) {
                v[xi1 + i] = xi[i];
                iv[a1 + i] = a[i];
        }
	k = (d + 1) * nv;
	for(i = 0; i < k; i++)
		v[vv1 + i] = vval[i];

	F77_SUB(ehg169)(&d, &vc, &nc, &nc, &nv, &nv, v+v1, iv+a1,
			v+xi1, iv+iv[7]-1, iv+iv[8]-1, iv+iv[9]-1);
}

void
loess_free()
{
        Free(v);
        Free(iv);
}

/* begin ehg's FORTRAN-callable C-codes */

void
F77_SUB(ehg182)(i)
  int *i;
{
  char *mess, mess2[50];
    switch(*i){
case 100: mess="wrong version number in lowesd.  Probably typo in caller."; break;
case 101: mess="d>dMAX in ehg131.  Need to recompile with increased dimensions."; break;
case 102: mess="liv too small.   (Discovered by lowesd)"; break;
case 103: mess="lv too small.    (Discovered by lowesd)"; break;
case 104: mess="span too small.  fewer data values than degrees of freedom."; break;
case 105: mess="k>d2MAX in ehg136.  Need to recompile with increased dimensions."; break;
case 106: mess="lwork too small"; break;
case 107: mess="invalid value for kernel"; break;
case 108: mess="invalid value for ideg"; break;
case 109: mess="lowstt only applies when kernel=1."; break;
case 110: mess="not enough extra workspace for robustness calculation"; break;
case 120: mess="zero-width neighborhood. make span bigger"; break;
case 121: mess="all data on boundary of neighborhood. make span bigger"; break;
case 122: mess="extrapolation not allowed with blending"; break;
case 123: mess="ihat=1 (diag L) in l2fit only makes sense if z=x (eval=data)."; break;
case 171: mess="lowesd must be called first."; break;
case 172: mess="lowesf must not come between lowesb and lowese, lowesr, or lowesl."; break;
case 173: mess="lowesb must come before lowese, lowesr, or lowesl."; break;
case 174: mess="lowesb need not be called twice."; break;
case 175: mess="need setLf=.true. for lowesl."; break;
case 180: mess="nv>nvmax in cpvert."; break;
case 181: mess="nt>20 in eval."; break;
case 182: mess="svddc failed in l2fit."; break;
case 183: mess="didnt find edge in vleaf."; break;
case 184: mess="zero-width cell found in vleaf."; break;
case 185: mess="trouble descending to leaf in vleaf."; break;
case 186: mess="insufficient workspace for lowesf."; break;
case 187: mess="insufficient stack space"; break;
case 188: mess="lv too small for computing explicit L"; break;
case 191: mess="computed trace L was negative; something is wrong!"; break;
case 192: mess="computed delta was negative; something is wrong!"; break;
case 193: mess="workspace in loread appears to be corrupted"; break;
case 194: mess="trouble in l2fit/l2tr"; break;
case 195: mess="only constant, linear, or quadratic local models allowed"; break;
case 196: mess="degree must be at least 1 for vertex influence matrix"; break;
case 999: mess="not yet implemented"; break;
default: sprintf(mess=mess2,"Assert failed; error code %d\n",*i); break;
    }
    warnmsg(mess);  
}

#include<string.h>
void
F77_SUB(ehg183)(s,i,n,inc)
  char *s;
  int *i, *n, *inc;
{
  char mess[4000], num[20];
  int j;
  strcpy(mess,s);
  for (j=0; j<*n; j++) {
    sprintf(num," %d",i[j * *inc]);
    strcat(mess,num);
  }
  strcat(mess,"\n");
  warnmsg(mess);  
}

void
F77_SUB(ehg184)(s,x,n,inc)
  char *s;
  double *x;
  int *n, *inc;
{
  char mess[4000], num[30];
  int j;
  strcpy(mess,s);
  for (j=0; j<*n; j++) {
    sprintf(num," %.5g",x[j * *inc]);
    strcat(mess,num);
  }
  strcat(mess,"\n");
  warnmsg(mess);  
}
