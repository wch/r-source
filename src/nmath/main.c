#include "Mathlib.h"
double gamma(double);

#include <floatingpoint.h>

main()
{
    double x, d;

    fpsetmask(0);
    x = -10.0;
    d = -1.0/0.0;
    printf("fprec(%.20g, %g) = %.20g\n", x, d, fprec(x, d));
}
