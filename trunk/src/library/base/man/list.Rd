\name{list}
\title{Lists -- Generic and Dotted Pairs}
\usage{
list(\dots)
pairlist(\dots)

as.list(x, \dots)
%as.list.default(x, \dots)
as.pairlist(x)
as.list.environment(x, all.names=FALSE, \dots)

is.list(x)
is.pairlist(x)

alist(\dots)
}
\alias{list}
\alias{pairlist}
\alias{alist}
\alias{as.list}
\alias{as.list.default}
\alias{as.list.data.frame}
\alias{as.list.environment}
\alias{as.pairlist}
\alias{is.list}
\alias{is.pairlist}
\description{
  Functions to construct, coerce and check for all kinds of \R lists.
}
\arguments{
  \item{\dots}{objects.}
  \item{x}{object to be coerced or tested.}
  \item{all.names}{a logical indicating whether to copy all values in
    \code{as.list.environment}/}
}
\details{
  Most lists in \R internally are \emph{Generic Vectors}, whereas
  traditional \emph{dotted pair} lists (as in LISP) are still available.

  The arguments to \code{list} or \code{pairlist} are of the form
  \code{value} or \code{tag=value}.
  The functions return a list composed of its arguments
  with each value either tagged or untagged,
  depending on how the argument was specified.

  \code{alist} is like \code{list}, except in the handling of tagged
  arguments with no value. These are handled as if they described
  function arguments with no default (cf. \code{\link{formals}}), whereas
  \code{list} simply ignores them.

  \code{as.list} attempts to coerce its argument to list type.
  For functions, this returns the concatenation of the list of formals
  arguments and the function body. For expressions, the list of
  constituent calls is returned.

  \code{is.list} returns \code{TRUE} iff its argument
  is a \code{list} \emph{or} a \code{pairlist} of \code{length} \eqn{> 0}.
  \code{is.pairlist} returns \code{TRUE} iff the argument is a pairlist
  or \code{NULL} (see below).

  \code{is.list} and \code{is.pairlist} are generic: you can write
  methods to handle specific classes of objects, see \link{InternalMethods}.

  \code{as.list.environment} copies the named values from an environment
  to a list.  The user can request that all named objects are copied
  (normally names that begin with a dot are not).  The output is not
  sorted and no parent environments are searched.

  An empty pairlist, \code{pairlist()} is the same as \code{NULL}.  This
  is different from \code{list()}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{vector}("list", length)} for creation of a list with empty
  components; \code{\link{c}}, for concatenation; \code{\link{formals}}.
}
\examples{
# create a plotting structure
pts <- list(x=cars[,1], y=cars[,2])
plot(pts)

## "pre-allocate" an empty list of length 5
vector("list", 5)

# Argument lists
f <- function()x
# Note the specification of a "..." argument:
formals(f) <- al <- alist(x=, y=2, \dots=)
f
al

## environment->list coercion

e1 <- new.env()
e1$a <- 10
e1$b <- 20
as.list(e1)
}
\keyword{list}
\keyword{manip}
