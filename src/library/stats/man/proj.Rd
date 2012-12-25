% File src/library/stats/man/proj.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{proj}
\title{Projections of Models}
\usage{
proj(object, \dots)

\method{proj}{aov}(object, onedf = FALSE, unweighted.scale = FALSE, \dots)

\method{proj}{aovlist}(object, onedf = FALSE, unweighted.scale = FALSE, \dots)

\method{proj}{default}(object, onedf = TRUE, \dots)

\method{proj}{lm}(object, onedf = FALSE, unweighted.scale = FALSE, \dots)
}
\alias{proj}
\alias{proj.default}
\alias{proj.lm}
\alias{proj.aov}
\alias{proj.aovlist}
\arguments{
 \item{object}{An object of class \code{"lm"} or a class inheriting from
   it, or an object with a similar structure including in particular
   components \code{qr} and \code{effects}.}
 \item{onedf}{A logical flag. If \code{TRUE}, a projection is returned for all
   the columns of the model matrix. If \code{FALSE}, the single-column
   projections are collapsed by terms of the model (as represented in
   the analysis of variance table).}
 \item{unweighted.scale}{If the fit producing \code{object} used
   weights, this determines if the projections correspond to weighted or
   unweighted observations.}
 \item{\dots}{Swallow and ignore any other arguments.}
}
\description{
 \code{proj} returns a matrix or list of matrices giving the projections
 of the data onto the terms of a linear model.  It is most frequently
 used for \code{\link{aov}} models.
}
\details{
 A projection is given for each stratum of the object, so for \code{aov}
 models with an \code{Error} term the result is a list of projections.
}
\value{
  A projection matrix or (for multi-stratum objects) a list of
  projection matrices.

  Each projection is a matrix with a row for each observations and
  either a column for each term (\code{onedf = FALSE}) or for each
  coefficient (\code{onedf = TRUE}). Projection matrices from the
  default method have orthogonal columns representing the projection of
  the response onto the column space of the Q matrix from the QR
  decomposition.  The fitted values are the sum of the projections, and
  the sum of squares for each column is the reduction in sum of squares
  from fitting that column (after those to the left of it).

  The methods for \code{lm} and \code{aov} models add a column to the
  projection matrix giving the residuals (the projection of the data
  onto the orthogonal complement of the model space).

  Strictly, when \code{onedf = FALSE} the result is not a projection,
  but the columns represent sums of projections onto the columns of the
  model matrix corresponding to that term. In this case the matrix does
  not depend on the coding used.
}
\references{
  Chambers, J. M., Freeny, A and Heiberger, R. M. (1992)
  \emph{Analysis of variance; designed experiments.}
  Chapter 5 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
}
\author{
  The design was inspired by the S function of the same name described
  in Chambers \emph{et al.} (1992).
}
\seealso{\code{\link{aov}}, \code{\link{lm}}, \code{\link{model.tables}}
}
\examples{
N <- c(0,1,0,1,1,1,0,0,0,1,1,0,1,1,0,0,1,0,1,0,1,1,0,0)
P <- c(1,1,0,0,0,1,0,1,1,1,0,0,0,1,0,1,1,0,0,1,0,1,1,0)
K <- c(1,0,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,0,1,1,1,0,1,0)
yield <- c(49.5,62.8,46.8,57.0,59.8,58.5,55.5,56.0,62.8,55.8,69.5,
55.0, 62.0,48.8,45.5,44.2,52.0,51.5,49.8,48.8,57.2,59.0,53.2,56.0)

npk <- data.frame(block = gl(6,4), N = factor(N), P = factor(P),
                  K = factor(K), yield = yield)
npk.aov <- aov(yield ~ block + N*P*K, npk)
proj(npk.aov)

## as a test, not particularly sensible
options(contrasts = c("contr.helmert", "contr.treatment"))
npk.aovE <- aov(yield ~  N*P*K + Error(block), npk)
proj(npk.aovE)
}
\keyword{models}
