#include <S.h>

static void *func;

static double zfun(double z)
{
    void *args[1];
    char *mode[1], *values[1];
    long length[1];    
    double zz[1], *result;

    mode[0] = "double"; length[0] = 1;
    args[0] = (void*)(zz); zz[0] = z;
    
    call_S(func, 1L, args, mode, length, 0L, 1L, values);
    
    result = (double*)values[0];
    return result[0];
}

static double zero_approx(double(*f)(), double x0, double x1, double tol)
{
    double f0, f1, fc, xc;
    f0 = zfun(x0);
    f1 = zfun(x1);
    if(f0 == 0.0) return x0;
    if(f1 == 0.0) return x1;
    if(f0*f1 > 0.0)
	error("x[0] and x[1] have the same sign\n");
    if(tol <= 0.0)
	error("non-positive tol value\n");
    for(;;) {
	xc = 0.5*(x0+x1);
	if(fabs(x0-x1) < tol) return xc;
	fc = zfun(xc);
	if(fc == 0) return xc;
	if(f0*fc > 0.0) {
	    x0 = xc;
	    f0 = fc;
	}
	else {
	    x1 = xc;
	    f1 = fc;
	}
    }
}

void zero_find(void *f, double *x, double *tol)
{
    func = f;
    x[0] = zero_approx(zfun, x[0], x[1], tol[0]);
    return;
}
