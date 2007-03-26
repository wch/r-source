\name{contrasts}
\alias{contrasts}
\alias{contrasts<-}
\title{Get and Set Contrast Matrices}
\description{
  Set and view the contrasts associated with a factor.
}
\usage{
contrasts(x, contrasts = TRUE)
contrasts(x, how.many) <- value
}
\arguments{
  \item{x}{a factor or a logical variable.}
  \item{contrasts}{logical. See Details.}
  \item{how.many}{How many contrasts should be made. Defaults to one
    less than the number of levels of \code{x}.  This need not be the
    same as the number of columns of \code{ctr}.}
  \item{value}{either a numeric matrix whose columns give coefficients for
    contrasts in the levels of \code{x}, or the (quoted) name of a
    function which computes such matrices.}
}
\details{
  If contrasts are not set for a factor the default functions from
  \code{\link{options}("contrasts")} are used.

  A logical vector \code{x} is converted into a two-level factor with
  levels \code{c(FALSE, TRUE)} (regardless of which levels occur in the
  variable).

  The argument \code{contrasts} is ignored if \code{x} has a matrix
  \code{contrasts} attribute set.  Otherwise if \code{contrasts = TRUE}
  it is passed to a contrasts function such as
  \code{\link{contr.treatment}} and if \code{contrasts = FALSE}
  an identity matrix is returned.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical models.}
  Chapter 2 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{C}},
  \code{\link{contr.helmert}},
  \code{\link{contr.poly}},
  \code{\link{contr.sum}},
  \code{\link{contr.treatment}};
  \code{\link{glm}},
  \code{\link{aov}},
  \code{\link{lm}}.
}
\examples{
example(factor)
fff <- ff[, drop=TRUE]  # reduce to 5 levels.
contrasts(fff) # treatment contrasts by default
contrasts(C(fff, sum))
contrasts(fff, contrasts = FALSE) # the 5x5 identity matrix

contrasts(fff) <- contr.sum(5); contrasts(fff)  # set sum contrasts
contrasts(fff, 2) <- contr.sum(5); contrasts(fff)  # set 2 contrasts
# supply 2 contrasts, compute 2 more to make full set of 4.
contrasts(fff) <- contr.sum(5)[,1:2]; contrasts(fff)
}
\keyword{design}
\keyword{regression}
