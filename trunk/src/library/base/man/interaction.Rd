\name{interaction}
\title{Compute Factor Interactions}
\usage{
interaction(\dots, drop = FALSE, sep = ".", lex.order = FALSE)
}
\alias{interaction}
\arguments{
  \item{\dots}{the factors for which interaction is to be computed, or a
    single list giving those factors.}
  \item{drop}{if \code{drop} is \code{TRUE}, unused factor levels
    are dropped from the result.  The default is to retain all
    factor levels.}
  \item{sep}{string to construct the new level labels by joining the
    constituent ones.}
  \item{lex.order}{logical indicating if the order of factor concatenation
    should be lexically ordered.}
}
\description{
  \code{interaction} computes a factor which represents the interaction
  of the given factors.  The result of \code{interaction} is always unordered.
}
\value{
  A factor which represents the interaction of the given factors.
  The levels are labelled as the levels of the individual factors joined
  by \code{sep} which is \code{.} by default.

  By default, when \code{lex.order = FALSE}, the levels are ordered so
  the level of the first factor varies fastest, then the second and so
  on.  This is the reverse of lexicographic ordering (which you can get
  by  \code{lex.order = TRUE}), and differs from
  \code{\link{:}}.  (It is done this way for compatibility with S.)
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{factor}};
  \code{\link{:}} where \code{f:g} is similar to
  \code{interaction(f, g, sep=":")} when \code{f} and  \code{g} are factors.
}
\examples{
a <- gl(2, 4, 8)
b <- gl(2, 2, 8, label = c("ctrl", "treat"))
s <- gl(2, 1, 8, label = c("M", "F"))
interaction(a, b)
interaction(a, b, s, sep = ":")
stopifnot(identical(a:s,
                    interaction(a, s, sep = ":", lex.order = TRUE)),
          identical(a:s:b,
                    interaction(a, s, b, sep = ":", lex.order = TRUE)))
}
\keyword{category}
