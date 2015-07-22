% File src/library/stats/man/contrasts.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{contrasts}
\alias{contrasts}
\alias{contrasts<-}
\title{Get and Set Contrast Matrices}
\description{
  Set and view the contrasts associated with a factor.
}
\usage{
contrasts(x, contrasts = TRUE, sparse = FALSE)
contrasts(x, how.many) <- value
}
\arguments{
  \item{x}{a factor or a logical variable.}
  \item{contrasts}{logical.  See \sQuote{Details}.}
  \item{sparse}{logical indicating if the result should be sparse
    (of class \code{\link[Matrix:dgCMatrix-class]{dgCMatrix}}), using
    package \CRANpkg{Matrix}.}
  \item{how.many}{How many contrasts should be made. Defaults to one
    less than the number of levels of \code{x}.  This need not be the
    same as the number of columns of \code{value}.}
  \item{value}{either a numeric matrix (or a sparse or dense matrix of a
    class extending \code{\link[Matrix:dMatrix-class]{dMatrix}} from
    package \CRANpkg{Matrix})  whose columns give coefficients for
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
  an identity matrix is returned.  Suitable functions have a first
  argument which is the character vector of levels, a named argument
  \code{contrasts} (always called with \code{contrasts = TRUE}) and
  optionally a logical argument \code{sparse}.

  If \code{value} supplies more than \code{how.many} contrasts, the
  first \code{how.many} are used.  If too few are supplied, a suitable
  contrast matrix is created by extending \code{value} after ensuring
  its columns are contrasts (orthogonal to the constant term) and not
  collinear.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical models.}
  Chapter 2 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{C}},% ./zC.Rd
  \code{\link{contr.helmert}},
  \code{\link{contr.poly}},
  \code{\link{contr.sum}},
  \code{\link{contr.treatment}};
  \code{\link{glm}},
  \code{\link{aov}},
  \code{\link{lm}}.
}
\examples{
utils::example(factor)
fff <- ff[, drop = TRUE]  # reduce to 5 levels.
contrasts(fff) # treatment contrasts by default
contrasts(C(fff, sum))
contrasts(fff, contrasts = FALSE) # the 5x5 identity matrix

contrasts(fff) <- contr.sum(5); contrasts(fff)  # set sum contrasts
contrasts(fff, 2) <- contr.sum(5); contrasts(fff)  # set 2 contrasts
# supply 2 contrasts, compute 2 more to make full set of 4.
contrasts(fff) <- contr.sum(5)[, 1:2]; contrasts(fff)

\donttest{## using sparse contrasts: % useful, once model.matrix() works with these :
ffs <- fff
contrasts(ffs) <- contr.sum(5, sparse = TRUE)[, 1:2]; contrasts(ffs)
stopifnot(all.equal(ffs, fff))
contrasts(ffs) <- contr.sum(5, sparse = TRUE); contrasts(ffs)
}}
\keyword{design}
\keyword{regression}
