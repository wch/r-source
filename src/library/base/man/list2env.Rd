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
         hash = FALSE, size = 29L)
}
\arguments{
  \item{x}{a \code{\link{list}}, where \code{\link{names}(x)} must
    contain valid names, i.e. at least of 1 character.}
  \item{envir}{an \code{\link{environment}} or \code{NULL}.}
  \item{parent}{(for the case \code{envir = NULL}): a parent frame aka
    enclosing environment, see \code{\link{new.env}}.}
  \item{hash}{(for the case \code{envir = NULL}): logical indicating
    if the created environment should use hashing, see \code{\link{new.env}}.}
  \item{size}{(in the case \code{envir = NULL, hash = TRUE}): hash size,
    see \code{\link{new.env}}.}
}
\details{
...
}
\value{
  an \code{\link{environment}}, either newly created (as with
  \code{\link{new.env}}) if the \code{envir} argument was \code{NULL},
  otherwise the updated environment \code{envir}.
}
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{Martin Maechler}
\seealso{
 \code{\link{environment}}, \code{\link{new.env}},
 \code{\link{as.environment}}; further, \code{\link{assign}}.

 The (semantical) \dQuote{inverse}: \code{\link{as.list.environment}}.
}
\examples{
L <- list(a=1, b=2:4, p = pi, ff = gl(3,4,labels=LETTERS[1:3]))
e <- list2env(L)
ls(e)
stopifnot(ls(e) == sort(names(L)),
          identical(L$b, e$b)) # "$" working for environments as for lists

## consistency, when we do the inverse:
ll <- as.list(e)# -> dispatching to the as.list.environment() method
rbind(names(L), names(ll)) # not in the same order, typically,
                           # but the same content:
stopifnot(identical(L [sort.list(names(L ))],
                    ll[sort.list(names(ll))]))

## now add to e -- can be seen as a fast "multi-assign":
e <- list2env(list(abc = LETTERS, note = "just an example",
                   df = data.frame(x=rnorm(20), y = rbinom(20,1, pr=0.2))),
              envir = e)
utils::ls.str(e)
}
\keyword{data}
