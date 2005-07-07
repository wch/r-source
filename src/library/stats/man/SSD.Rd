\name{SSD}
\alias{SSD}
\alias{estVar}
\title{SSD Matrix and Estimated Variance Matrix in Multivariate Models}
\description{
  Functions to compute matrix of residual sums of squares and products,
  or the estimated variance matrix for multivariate linear models.
}
\usage{
% Cheating! - the S3 methods are hidden ... \method{SSD}{mlm}(object, ...)
# S3 method for class 'mlm'
SSD(object, ...)

# S3 methods for class 'SSD' and 'mlm'
estVar(object, ...)
}
\arguments{
  \item{object}{\code{object} of class \code{"mlm"}, or \code{"SSD"} in
    the case of \code{estVar}.}
  \item{\dots}{Unused}
}
\value{
  \code{SSD()} returns a list of class \code{"SSD"} containing the
  following components
  \item{SSD}{The residual sums of squares and products matrix}
  \item{df}{Degrees of freedom}
  \item{call}{Copied from \code{object}}

  \code{estVar} returns a matrix with the estimated variances and
  covariances.
}
\seealso{\code{\link{mauchley.test}}, \code{\link{anova.mlm}}}
%% TODO: Have this in datasets w/full docs
\examples{
# Lifted from Baron+Li:
# "Notes on the use of R for psychology experiments and questionnaires"
# Maxwell and Delaney, p. 497
reacttime <- matrix(c(
420, 420, 480, 480, 600, 780,
420, 480, 480, 360, 480, 600,
480, 480, 540, 660, 780, 780,
420, 540, 540, 480, 780, 900,
540, 660, 540, 480, 660, 720,
360, 420, 360, 360, 480, 540,
480, 480, 600, 540, 720, 840,
480, 600, 660, 540, 720, 900,
540, 600, 540, 480, 720, 780,
480, 420, 540, 540, 660, 780),
ncol = 6, byrow = TRUE,
dimnames=list(subj=1:10,
              cond=c("deg0NA", "deg4NA", "deg8NA",
                     "deg0NP", "deg4NP", "deg8NP")))

mlmfit <- lm(reacttime~1)
SSD(mlmfit)
estVar(mlmfit)
}
\keyword{models}
\keyword{multivariate}
