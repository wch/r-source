% File src/library/stats/man/relevel.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007, 2016 R Core Team
% Distributed under GPL 2 or later

\name{relevel}
\alias{relevel}
\alias{relevel.default}
\alias{relevel.factor}
\alias{relevel.ordered}
\title{Reorder Levels of Factor}
\usage{
relevel(x, ref, \dots)
}
\arguments{
  \item{x}{an unordered factor.}
  \item{ref}{the reference level, typically a string.}
  \item{\dots}{additional arguments for future methods.}
}
\description{
    The levels of a factor are re-ordered so that the level specified by
    \code{ref} is first and the others are moved down. This is useful
    for \code{contr.treatment} contrasts which take the first level as
    the reference.
}
\value{
  A factor of the same length as \code{x}.
}
\details{
  This, as \code{\link{reorder}()}, is a special case of simply calling
  \code{\link{factor}(x, levels = levels(x)[....])}.
}
\seealso{
  \code{\link{factor}}, \code{\link{contr.treatment}},
  \code{\link{levels}}, \code{\link{reorder}}.
}

\examples{
warpbreaks$tension <- relevel(warpbreaks$tension, ref = "M")
summary(lm(breaks ~ wool + tension, data = warpbreaks))
}
\keyword{utilities}
\keyword{models}
