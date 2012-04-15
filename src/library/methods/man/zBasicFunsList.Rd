% File src/library/methods/man/zBasicFunsList.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{.BasicFunsList}
\alias{.BasicFunsList}
\title{List of Builtin and Special Functions}
\description{
  A named list providing instructions for turning builtin and special
  functions into generic functions.

  Functions in R that are defined as \code{.Primitive(<name>)} are not
  suitable for formal methods, because they lack the basic reflectance
  property.  You can't find the argument list for these functions by
  examining the function object itself.

  Future versions of R may fix this by attaching a formal argument list
  to the corresponding function.  While generally the names of arguments
  are not checked by the internal code implementing the function, the
  number of arguments frequently is.

  In any case, some definition of a formal argument list is needed if
  users are to define methods for these functions.  In particular, if
  methods are to be merged from multiple packages, the different sets
  of methods need to agree on the formal arguments.

  In the absence of reflectance, this list provides the relevant
  information  via a dummy function associated with each of the known
  specials for which methods are allowed.

  At the same, the list flags those specials for which methods are
  meaningless (e.g., \code{for}) or just a very bad idea (e.g.,
  \code{.Primitive}).

  A generic function created via \code{\link{setMethod}}, for
  example, for one of these special functions will have the argument
  list from \code{.BasicFunsList}.  If no entry exists, the argument
  list \code{(x, ...)}  is assumed.
}
\keyword{ programming }
\keyword{ methods }
