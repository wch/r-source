\name{Random.user}
\title{User-supplied Random Number Generation}
\description{
  Function \code{\link{RNGkind}} allows a user-coded random number
  generator to be supplied. The details are give here.
}
\details{
  A user-specified RNG is called from entry points in dynamically-loaded
  compiled code. The user must supply the entry point
  \code{user_unif_rand}, which takes no arguments and returns a
  \emph{pointer to} a double.

  Optionally, the user can supply the entry point \code{user_unif_init},
  which is called with an \code{unsigned int} argument when
  \code{\link{RNGkind}} (or \code{set.seed}) is called, and is intended
  to be used to initialize the user's RNG code. The argument is intended
  to be used to set the ``seeds''; it is the \code{seed} argument to
  \code{set.seed} or an essentially random seed if \code{\link{RNGkind}}
  is called.

  If only these functions are supplied, no information about the
  generator's state is recorded in \code{.Random.seed}. Optionally,
  functions \code{user_unif_nseed} and \code{user_unif_seedloc} can be
  supplied which are called with no arguments and should return the
  number of ``seeds'' and a pointer to an integer array of ``seeds''.
}
\section{Warning}{As with all compiled code, mis-specifying these
  functions can crash \R. Do include the \file{R_ext/Random.h}
  header file for some type-checking.
}
\examples{\dontrun{
## Marsaglia's conguential PRNG
#include <R_ext/Random.h>

static Int32 seed;
static double res;

double * user_unif_rand()
{
    seed = 69069 * seed + 1;
    res = seed * 2.32830643653869e-10;
    return &res;
}

void  user_unif_init(Int32 seed_in) { seed = seed_in; }
int   user_unif_nseed() { return 1; }
int * user_unif_seedloc() { return (int *) &seed; }
}}