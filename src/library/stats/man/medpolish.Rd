% File src/library/stats/man/medpolish.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{medpolish}
\alias{medpolish}
\title{Median Polish of a Matrix}
\description{
  Fits an additive model using Tukey's \emph{median polish} procedure.
}
\usage{
medpolish(x, eps = 0.01, maxiter = 10, trace.iter = TRUE,
          na.rm = FALSE)
}
\arguments{
  \item{x}{a numeric matrix.}
  \item{eps}{real number greater than 0. A tolerance for convergence:
    see \sQuote{Details}.}
  \item{maxiter}{the maximum number of iterations}
  \item{trace.iter}{logical. Should progress in convergence be reported?}
  \item{na.rm}{logical. Should missing values be removed?}
}
\details{
  The model fitted is additive (constant + rows + columns). The
  algorithm works by alternately removing the row and column medians,
  and continues until the proportional reduction in the sum
  of absolute residuals is less than \code{eps}
  or until there have been \code{maxiter} iterations.
  The sum of absolute residuals is printed at
  each iteration of the fitting process, if \code{trace.iter} is \code{TRUE}.
  If \code{na.rm} is \code{FALSE} the presence of any \code{NA} value in
  \code{x} will cause an error, otherwise \code{NA} values are ignored.

  \code{medpolish} returns an object of class \code{medpolish} (see below).
  There are printing and plotting methods for this
  class, which are invoked via by the generics
  \code{\link{print}} and \code{\link{plot}}.
}
\value{
  An object of class \code{medpolish} with the following named components:
  \item{overall}{the fitted constant term.}
  \item{row}{the fitted row effects.}
  \item{col}{the fitted column effects.}
  \item{residuals}{the residuals.}
  \item{name}{the name of the dataset.}
}
\seealso{\code{\link{median}}; \code{\link{aov}} for a \emph{mean}
  instead of \emph{median} decomposition.}
\references{
  Tukey, J. W. (1977).
  \emph{Exploratory Data Analysis},
  Reading Massachusetts: Addison-Wesley.
}
\examples{
require(graphics)

## Deaths from sport parachuting;  from ABC of EDA, p.224:
deaths <-
    rbind(c(14,15,14),
          c( 7, 4, 7),
          c( 8, 2,10),
          c(15, 9,10),
          c( 0, 2, 0))
dimnames(deaths) <- list(c("1-24", "25-74", "75-199", "200++", "NA"),
                         paste(1973:1975))
deaths
(med.d <- medpolish(deaths))
plot(med.d)
## Check decomposition:
all(deaths ==
    med.d$overall + outer(med.d$row,med.d$col, "+") + med.d$residuals)
}
\keyword{robust}
