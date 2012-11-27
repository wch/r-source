% File src/library/base/man/list2env.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2010 R Core Team
% Distributed under GPL 2 or later

\name{list2env}
\alias{list2env}
\title{From A List, Build or Add To an Environment}
\description{
  From a \emph{named} \code{\link{list} x}, create an
  \code{\link{environment}} containing all list components as objects, or
  \dQuote{multi-assign} from \code{x} into a pre-existing environment.
}
\usage{
list2env(x, envir = NULL, parent = parent.frame(),
         hash = (length(x) > 100), size = max(29L, length(x)))
}
\arguments{
  \item{x}{a \code{\link{list}}, where \code{\link{names}(x)} must
    not contain empty (\code{""}) elements.}
  \item{envir}{an \code{\link{environment}} or \code{NULL}.}
  \item{parent}{(for the case \code{envir = NULL}): a parent frame aka
    enclosing environment, see \code{\link{new.env}}.}
  \item{hash}{(for the case \code{envir = NULL}): logical indicating
    if the created environment should use hashing, see \code{\link{new.env}}.}
  \item{size}{(in the case \code{envir = NULL, hash = TRUE}): hash size,
    see \code{\link{new.env}}.}
}
\details{
  This will be very slow for large inputs unless hashing is used on the
  environment.

  Environments must have uniquely named entries, but named lists need
  not: where the list has duplicate names it is the \emph{last} element
  with the name that is used.  Empty names throw an error.
}
\value{
  An \code{\link{environment}}, either newly created (as by
  \code{\link{new.env}}) if the \code{envir} argument was \code{NULL},
  otherwise the updated environment \code{envir}.  Since environments
  are never duplicated, the argument \code{envir} is also changed.
}
\author{Martin Maechler}
\seealso{
  \code{\link{environment}}, \code{\link{new.env}},
  \code{\link{as.environment}}; further, \code{\link{assign}}.

  The (semantical) \dQuote{inverse}: \code{\link{as.list.environment}}.
}
\examples{
L <- list(a = 1, b = 2:4, p = pi, ff = gl(3, 4, labels = LETTERS[1:3]))
e <- list2env(L)
ls(e)
stopifnot(ls(e) == sort(names(L)),
          identical(L$b, e$b)) # "$" working for environments as for lists

## consistency, when we do the inverse:
ll <- as.list(e)  # -> dispatching to the as.list.environment() method
rbind(names(L), names(ll)) # not in the same order, typically,
                           # but the same content:
stopifnot(identical(L [sort.list(names(L ))],
                    ll[sort.list(names(ll))]))

## now add to e -- can be seen as a fast "multi-assign":
list2env(list(abc = LETTERS, note = "just an example",
              df = data.frame(x = rnorm(20), y = rbinom(20, 1, pr = 0.2))),
         envir = e)
utils::ls.str(e)
}
\keyword{data}
