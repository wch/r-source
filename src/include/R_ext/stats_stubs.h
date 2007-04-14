#include <Rconfig.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#ifdef HAVE_VISIBILITY_ATTRIBUTE
# define attribute_hidden __attribute__ ((visibility ("hidden")))
#else
# define attribute_hidden
#endif

void attribute_hidden
S_Rf_divset(int alg, int iv[], int liv, int lv, double v[])
{
    static void(*fun)(int,int[],int,int,double[]) = NULL;
    if (fun == NULL)
	fun = (void(*)(int,int[],int,int,double[]))
	    R_GetCCallable("stats", "Rf_divset");
    fun(alg, iv, liv, lv, v);
}

void attribute_hidden
S_nlminb_iterate(double b[], double d[], double fx, double g[], double h[],
		 int iv[], int liv, int lv, int n, double v[], double x[])
{
    static void(*fun)(double[],double[],double,double[],double[],
		      int[],int,int,int,double[],double[]) = NULL;
    if (fun == NULL)
	fun = (void(*)(double[],double[],double,double[],double[],
			  int[],int,int,int,double[],double[]))
	    R_GetCCallable("stats", "nlminb_iterate");
    fun(b, d, fx, g, h, iv, liv, lv, n, v, x);
}

void attribute_hidden
S_nlsb_iterate(double b[], double d[], double dr[], int iv[], int liv,
	       int lv, int n, int nd, int p, double r[], double rd[],
	       double v[], double x[])
{
    static void(*fun)(double[],double[],double[],int[],int,int,
		      int,int,int,double[],double[],double[],
		      double[]) = NULL;
    if (fun == NULL)
	fun = (void(*)(double[],double[],double[],int[],int,
		       int, int,int,int,double[],
		       double[],double[],double[]))
	    R_GetCCallable("stats", "nlsb_iterate");
    fun(b, d, dr, iv, liv, lv, n, nd, p, r, rd, v, x);
}

