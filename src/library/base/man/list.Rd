\name{list}
\alias{list}
\alias{pairlist}
\alias{alist}
\alias{as.list}
\alias{as.list.default}
\alias{as.list.data.frame}
\alias{as.list.environment}
\alias{as.list.factor}
\alias{as.pairlist}
\alias{is.list}
\alias{is.pairlist}
\title{Lists -- Generic and Dotted Pairs}
\description{
  Functions to construct, coerce and check for both kinds of \R lists.
}
\usage{
list(\dots)
pairlist(\dots)

as.list(x, \dots)
\method{as.list}{environment}(x, all.names = FALSE, \dots)
as.pairlist(x)

is.list(x)
is.pairlist(x)

alist(\dots)
}
\arguments{
  \item{\dots}{objects, possibly named.}
  \item{x}{object to be coerced or tested.}
  \item{all.names}{a logical indicating whether to copy all values or
    (default) only those whose names do not begin with a dot.}
}
\details{
  Most lists in \R internally are \emph{Generic Vectors}, whereas
  traditional \emph{dotted pair} lists (as in LISP) are available
  but rarely seen by users (except as \code{\link{formals}} of functions).

  The arguments to \code{list} or \code{pairlist} are of the form
  \code{value} or \code{tag=value}.  The functions return a list or
  dotted pair list composed of its arguments with each value either
  tagged or untagged, depending on how the argument was specified.

  \code{alist} handles its arguments as if they described function
  arguments.  So the values are not evaluated, and tagged arguments with
  no value are allowed whereas \code{list} simply ignores them.
  \code{alist} is most often used in conjunction with \code{\link{formals}}.

  \code{as.list} attempts to coerce its argument to a list.  For
  functions, this returns the concatenation of the list of formal
  arguments and the function body.  For expressions, the list of
  constituent elements is returned.  \code{as.list} is generic, and as the
  default method calls \code{\link{as.vector}(mode="list")} methods for
  \code{as.vector} may be invoked.  As from \R 2.4.0, \code{as.list}
  turns a factor into a list of one-element factors.  All attributes
  will be dropped unless the argument already is a list.  (This is
  inconsistent with functions such as \code{\link{as.character}}, and is
  for efficiency since lists can be expensive to copy.)

  \code{is.list} returns \code{TRUE} if and only if its argument
  is a \code{list} \emph{or} a \code{pairlist} of \code{length} \eqn{> 0}.
  \code{is.pairlist} returns \code{TRUE} if and only if the argument
  is a pairlist or \code{NULL} (see below).

  The \code{"\link{environment}"} method for \code{as.list} copies the
  name-value pairs (for names not beginning with a dot) from an
  environment to a named list.  The user can request that all named
  objects are copied.  The list is in no particular order (the order
  depends on the order of creation of objects and whether the
  environment is hashed).  No parent environments are searched.  (Objects
  copied are duplicated so this can be an expensive operation.)

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
  \code{\link{unlist}} is an approximate inverse to \code{as.list()}.
}
\examples{
require(graphics)

# create a plotting structure
pts <- list(x=cars[,1], y=cars[,2])
plot(pts)

is.pairlist(.Options)  # a user-level pairlist

## "pre-allocate" an empty list of length 5
vector("list", 5)

# Argument lists
f <- function() x
# Note the specification of a "..." argument:
formals(f) <- al <- alist(x=, y=2+3, \dots=)
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
