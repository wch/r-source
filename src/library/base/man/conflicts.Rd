% File src/library/base/man/conflicts.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{conflicts}
\title{Search for Masked Objects on the Search Path}
\usage{
conflicts(where = search(), detail = FALSE)
}
\alias{conflicts}
\arguments{
 \item{where}{A subset of the search path, by default the whole search path.}
 \item{detail}{If \code{TRUE}, give the masked or masking functions for
   all members of the search path.}
}
\description{
    \code{conflicts} reports on objects that exist with the same name in
    two or more places on the \code{\link{search}} path, usually because
    an object in the user's workspace or a package is masking a system
    object of the same name.  This helps discover unintentional masking.
}
\value{
    If \code{detail = FALSE}, a character vector of masked objects.
    If \code{detail = TRUE}, a list of character vectors giving the masked or
    masking objects in that member of the search path.  Empty vectors are
    omitted.
}
\examples{
lm <- 1:3
conflicts(, TRUE)
## gives something like
# $.GlobalEnv
# [1] "lm"
#
# $package:base
# [1] "lm"

## Remove things from your "workspace" that mask others:
remove(list = conflicts(detail = TRUE)$.GlobalEnv)
}
\keyword{utilities}
