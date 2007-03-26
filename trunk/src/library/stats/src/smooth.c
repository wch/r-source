/* Tukey Median Smoothing */

#include <stdlib.h> /* for abs */
#include <math.h>
#include <R_ext/Boolean.h>
#include <R_ext/Error.h>
#include <R_ext/Memory.h>

#ifdef DEBUG_smooth
# include <R_ext/PrtUtil.h>
#endif

#include "eda.h"
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("stats", String)
#else
#define _(String) (String)
#endif

static double med3(double u, double v, double w)
{
    /* Median(u,v,w): */
    if((u <= v && v <= w) ||
       (u >= v && v >= w)) return v;
    if((u <= w && w <= v) ||
       (u >= w && w >= v)) return w;
    /* else */ return u;
}
/* Note: Velleman & Hoaglin use a smarter version,  which returns "change"
   ----
   and change = TRUE, when  med3(u,v,w) != v   ==> makes "R" (in "3R") faster
*/
static int imed3(double u, double v, double w)
{
    /* Return (Index-1) of  median(u,v,w) , i.e.,
       -1 : u
        0 : v
	1 : w
     */
    if((u <= v && v <= w) ||
       (u >= v && v >= w)) return 0;
    if((u <= w && w <= v) ||
       (u >= w && w >= v)) return 1;
    /* else */ return -1;
}

static Rboolean sm_3(double *x, double *y, int n, int end_rule)
{
    /* y[] := Running Median of three (x) = "3 (x[])" with "copy ends"
     * ---  return chg := ( y != x ) */
    int i,j;
    Rboolean chg = FALSE;

    for(i = 1; i < n-1; i++) {
 	j = imed3(x[i-1], x[i], x[i+1]);
	y[i] = x[i + j];
	chg = chg || j;
    }
/* [y, chg]  :=  sm_DO_ENDRULE(x, y, end_rule, chg) : */
#define sm_DO_ENDRULE(y)						\
    switch(end_rule) {							\
	   case sm_NO_ENDRULE:						\
	   /* do nothing : don't even assign them */ break;		\
									\
	   case sm_COPY_ENDRULE:					\
	   y[0] = x[0];							\
	   y[n-1] = x[n-1];						\
	   break;							\
									\
	   case sm_TUKEY_ENDRULE:					\
	   y[0] = med3(3*y[1] - 2*y[2], x[0], y[1]);			\
	   chg = chg || (y[0] != x[0]);					\
	   y[n-1] = med3(y[n-2], x[n-1], 3*y[n-2] - 2*y[n-3]);		\
	   chg = chg || (y[n-1] != x[n-1]);				\
	   break;							\
									\
	   default:							\
	   error(_("invalid end-rule for running median of 3: %d"),	\
		 end_rule);						\
    }

    sm_DO_ENDRULE(y);

    return chg;
}

static int sm_3R(double *x, double *y, double *z, int n, int end_rule)
{
    /* y[] := "3R"(x) ; 3R = Median of three, repeated until convergence */
    int i, iter; 
    Rboolean chg;

    iter = chg = sm_3(x, y, n, sm_COPY_ENDRULE);

    while(chg) {
	if((chg = sm_3(y, z, n, sm_NO_ENDRULE))) {
	    iter++;
	    for(i=1; i < n-1; i++)
		y[i] = z[i];
	}
    }

    sm_DO_ENDRULE(y);/* =>  chg = TRUE  iff  ends changed */

    return(iter ? iter : chg);
    /* = 0   <==>  only one "3" w/o any change
       = 1   <==>  either ["3" w/o change + endchange]
		   or	  [two "3"s, 2nd w/o change  ]
    */
}


static Rboolean sptest(double *x, int i)
{
    /* Split test:
       Are we at a /-\ or \_/ location => split should be made ?
     */
    if(x[i] != x[i+1]) return FALSE;
    if((x[i-1] <= x[i] && x[i+1] <= x[i+2]) ||
       (x[i-1] >= x[i] && x[i+1] >= x[i+2])) return FALSE;
    /* else */ return TRUE;
}


static Rboolean sm_split3(double *x, double *y, int n, Rboolean do_ends)
{
    /* y[] := S(x[])  where S() = "sm_split3"  */
    int i, j; 
    Rboolean chg = FALSE;

    for(i=0; i < n; i++)
	y[i] = x[i];

    /* Colin Goodall doesn't do splits near ends
       in spl() in Statlib's "smoother" code !! */
    if(do_ends && sptest(x, 1)) {
	chg = TRUE;
	y[1] = x[0];
	y[2] = med3(x[2], x[3], 3*x[3] - 2*x[4]);
    }

    for(i=2; i < n-3; i++)
	if(sptest(x, i)) { /* plateau at x[i] == x[i+1] */
	    /* at left : */
	    if(-1 < (j = imed3(x[i ], x[i-1], 3*x[i-1] - 2*x[i-2]))) {
		y[i]   = /* med3(.) = */ (j == 0)? x[i-1] : 3*x[i-1] - 2*x[i-2];
		chg = y[i] != x[i];
	    }
	    /* at right : */
	    if(-1 < (j = imed3(x[i+1], x[i+2], 3*x[i+2] - 2*x[i+3]))) {
		y[i+1] = /* med3(.) = */ (j == 0)? x[i+2] : 3*x[i+2] - 2*x[i+3];
		chg = y[i+1] != x[i+1];
	    }
	}
    if(do_ends && sptest(x, n-3)) {
	chg = TRUE;
	y[n-2] = x[n-1];
	y[n-3] = med3(x[n-3], x[n-4], 3*x[n-4] - 2*x[n-5]);
    }
    return(chg);
}

static int sm_3RS3R(double *x, double *y, double *z, int n,
	     int end_rule, Rboolean split_ends)
{
    /* y[1:n] := "3R S 3R"(x[1:n]);  z = "work"; */
    int iter;
    Rboolean chg;

    iter =  sm_3R    (x, y, z, n, end_rule);
    chg  =  sm_split3(y, z, n, split_ends);
    if(chg)
	iter += sm_3R(z, y, x, n, end_rule);
    /* else y == z already */
    return(iter + (int)chg);
}

static int sm_3RSS(double *x, double *y, double *z, int n,
	    int end_rule, Rboolean split_ends)
{
    /* y[1:n] := "3RSS"(x[1:n]);  z = "work"; */
    int iter;
    Rboolean chg;

    iter = sm_3R    (x, y, z, n, end_rule);
    chg =  sm_split3(y, z, n, split_ends);
    if(chg)
	sm_split3(z, y, n, split_ends);
    /* else  y == z already */
    return(iter + (int)chg);
}

static int sm_3RSR(double *x, double *y, double *z, double *w, int n,
	    int end_rule, Rboolean split_ends)
{
    /* y[1:n] := "3RSR"(x[1:n]);  z := residuals; w = "work"; */

/*== "SR" (as follows) is stupid ! (MM) ==*/

    int i, iter;
    Rboolean chg, ch2;

    iter = sm_3R(x, y, z, n, end_rule);

    do {
	iter++;
	chg = sm_split3(y, z, n, split_ends);
	ch2 = sm_3R(z, y, w, n, end_rule);
	chg = chg || ch2;

	if(!chg) break;
	if(iter > 2*n) break;/* INF.LOOP stopper */
	for(i=0; i < n; i++)
	    z[i] = x[i] - y[i];

    } while (chg);

    return(iter);
}


/*-------- These are  called from R : -----------*/

/* the `bad' one (default in R <= 1.1): */
void Rsm_3RSR(double *x, double *y, int *n, int *end_rule, int *iter)
{
    double *z = (double *) R_alloc(*n, sizeof(double));
    double *w = (double *) R_alloc(*n, sizeof(double));
    if(!z || !w)
	error(_("allocation error in smooth(*, '3RSR')."));

    *iter = sm_3RSR(x, y, z, w, *n, abs(*end_rule),
		    /* split_ends = */(*end_rule < 0)? TRUE : FALSE);
    return;
}

void Rsm_3RS3R(double *x, double *y, int *n, int *end_rule, int *changed)
{
    double *z = (double *) R_alloc(*n, sizeof(double));
    if(!z)
	error(_("allocation error in smooth(*, '3RSS')."));
    *changed = sm_3RS3R(x, y, z, *n, abs(*end_rule),
			/* split_ends = */(*end_rule < 0)? TRUE : FALSE);
    return;
}

void Rsm_3RSS(double *x, double *y, int *n, int *end_rule, int *changed)
{
    double *z = (double *) R_alloc(*n, sizeof(double));
    if(!z)
	error(_("allocation error in smooth(*, '3RSS')."));
    *changed = sm_3RSS(x, y, z, *n, abs(*end_rule),
		    /* split_ends = */(*end_rule < 0)? TRUE : FALSE);
    return;
}

void Rsm_3R(double *x, double *y, int *n, int *end_rule, int *iter)
{
    double *z = (double *) R_alloc(*n, sizeof(double));
    if(!z)
	error(_("allocation error in smooth(*, '3R')."));
    *iter = sm_3R(x, y, z, *n, *end_rule);
    return;
}

void Rsm_3(double *x, double *y, int *n, int *end_rule, int *changed)
{
    *changed = sm_3(x, y, *n, *end_rule);
    return;
}

void Rsm_S(double *x, double *y, int *n, int *do_ends, int *changed)
{
    *changed = sm_split3(x, y, *n, (Rboolean)*do_ends);
    return;
}
