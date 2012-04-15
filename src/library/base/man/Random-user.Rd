% File src/library/base/man/Random-user.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{Random.user}
\title{User-supplied Random Number Generation}
\alias{Random.user}
\description{
  Function \code{\link{RNGkind}} allows user-coded uniform and
  normal random number generators to be supplied.  The details are given
  here.
}
\details{
  A user-specified uniform RNG is called from entry points in
  dynamically-loaded compiled code.  The user must supply the entry point
  \code{user_unif_rand}, which takes no arguments and returns a
  \emph{pointer to} a double. The example below will show the general
  pattern.

  Optionally, the user can supply the entry point \code{user_unif_init},
  which is called with an \code{unsigned int} argument when
  \code{\link{RNGkind}} (or \code{set.seed}) is called, and is intended
  to be used to initialize the user's RNG code.  The argument is intended
  to be used to set the \sQuote{seeds}; it is the \code{seed} argument to
  \code{set.seed} or an essentially random seed if \code{\link{RNGkind}}
  is called.

  If only these functions are supplied, no information about the
  generator's state is recorded in \code{.Random.seed}.  Optionally,
  functions \code{user_unif_nseed} and \code{user_unif_seedloc} can be
  supplied which are called with no arguments and should return pointers
  to the number of seeds and to an integer (specifically, \samp{Int32})
  array of seeds.  Calls to \code{GetRNGstate} and \code{PutRNGstate}
  will then copy this array to and from \code{.Random.seed}.

  A user-specified normal RNG is specified by a single entry point
  \code{user_norm_rand}, which takes no arguments and returns a
  \emph{pointer to} a double.
}
\section{Warning}{As with all compiled code, mis-specifying these
  functions can crash \R.  Do include the \file{R_ext/Random.h}
  header file for type checking.
}
\examples{\dontrun{
##  Marsaglia's congruential PRNG
#include <R_ext/Random.h>

static Int32 seed;
static double res;
static int nseed = 1;

double * user_unif_rand()
{
    seed = 69069 * seed + 1;
    res = seed * 2.32830643653869e-10;
    return &res;
}

void  user_unif_init(Int32 seed_in) { seed = seed_in; }
int * user_unif_nseed() { return &nseed; }
int * user_unif_seedloc() { return (int *) &seed; }

/*  ratio-of-uniforms for normal  */
#include <math.h>
static double x;

double * user_norm_rand()
{
    double u, v, z;
    do {
        u = unif_rand();
        v = 0.857764 * (2. * unif_rand() - 1);
        x = v/u; z = 0.25 * x * x;
        if (z < 1. - u) break;
        if (z > 0.259/u + 0.35) continue;
    } while (z > -log(u));
    return &x;
}

## Use under Unix:
R CMD SHLIB urand.c
R
> dyn.load("urand.so")
> RNGkind("user")
> runif(10)
> .Random.seed
> RNGkind(, "user")
> rnorm(10)
> RNGkind()
[1] "user-supplied" "user-supplied"
}}
\keyword{distribution}
\keyword{sysdata}
