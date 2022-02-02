% File src/library/base/man/unlist.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{unlist}
\title{Flatten Lists}
\alias{unlist}
\description{
  Given a list structure \code{x}, \code{unlist} simplifies it to
  produce a vector which contains all the atomic components
  which occur in \code{x}.
}
\usage{
unlist(x, recursive = TRUE, use.names = TRUE)
}
\arguments{
  \item{x}{an \R object, typically a list or vector.}
  \item{recursive}{logical.  Should unlisting be applied to list
    components of \code{x}?}
  \item{use.names}{logical.  Should names be preserved?}
}
\details{
  \code{unlist} is generic: you can write methods to handle
  specific classes of objects, see \link{InternalMethods},
  and note, e.g., \code{\link{relist}} with the \code{unlist} method
  for \code{relistable} objects.

  If \code{recursive = FALSE}, the function will not recurse beyond the
  first level items in \code{x}.

  Factors are treated specially.  If all non-list elements of \code{x}
  are \code{\link{factor}} (or ordered factor) objects then the result
  will be a factor with
  levels the union of the level sets of the elements, in the order the
  levels occur in the level sets of the elements (which means that if
  all the elements have the same level set, that is the level set of the
  result).

  \code{x} can be an atomic vector, but then \code{unlist} does nothing useful,
  not even drop names.

  By default, \code{unlist} tries to retain the naming
  information present in \code{x}.  If \code{use.names = FALSE} all
  naming information is dropped.

  Where possible the list elements are coerced to a common mode during
  the unlisting, and so the result often ends up as a character
  vector.  Vectors will be coerced to the highest type of the components
  in the hierarchy NULL < raw < logical < integer < double < complex < character
  < list < expression: pairlists are treated as lists.

  A list is a (generic) vector, and the simplified vector might still be
  a list (and might be unchanged).  Non-vector elements of the list
  (for example language elements such as names, formulas and calls)
  are not coerced, and so a list containing one or more of these remains a
  list.  (The effect of unlisting an \code{\link{lm}} fit is a list which
  has individual residuals as components.)
  Note that \code{unlist(x)} now returns \code{x} unchanged also for
  non-vector \code{x}, instead of signalling an error in that case.
}
\value{
  \code{NULL} or an expression or a vector of an appropriate mode to
  hold the list components.

  The output type is determined from the highest type
  of the components in the hierarchy NULL < raw < logical < integer < double <
  complex < character < list < expression, after coercion of pairlists
  to lists.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{c}}, \code{\link{as.list}}, \code{\link{relist}}.
}
\examples{
unlist(options())
unlist(options(), use.names = FALSE)

l.ex <- list(a = list(1:5, LETTERS[1:5]), b = "Z", c = NA)
unlist(l.ex, recursive = FALSE)
unlist(l.ex, recursive = TRUE)

l1 <- list(a = "a", b = 2, c = pi+2i)
unlist(l1) # a character vector
l2 <- list(a = "a", b = as.name("b"), c = pi+2i)
unlist(l2) # remains a list

ll <- list(as.name("sinc"), quote( a + b ), 1:10, letters, expression(1+x))
utils::str(ll)
for(x in ll)
  stopifnot(identical(x, unlist(x)))
}
\keyword{list}
\keyword{manip}
