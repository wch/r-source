% File src/library/stats/man/profile.Rd
% Part of the R package, https://www.R-project.org

% Originally:
% file MASS/man/profile.glm.Rd
% copyright (C) 1999-2008 W. N. Venables and B. D. Ripley

% Modified for Rao Score test ("test=" argument) by Peter Dalgaard 2023
% Changes Copyright R Core Team 2023

\name{profile.glm}
\alias{profile.glm}
\title{Method for Profiling \code{glm} Objects}
\description{
  Investigates the profile log-likelihood function for a fitted model of
  class \code{"glm"}.
}
\usage{
\S3method{profile}{glm}(fitted, which = 1:p, alpha = 0.01, maxsteps = 10,
        del = zmax/5, trace = FALSE, test = c("LRT", "Rao"), \dots)
}
\arguments{
  \item{fitted}{the original fitted model object.}
  \item{which}{the original model parameters which should be profiled.
    This can be a numeric or character vector.
    By default, all parameters are profiled.}
  \item{alpha}{highest significance level allowed for the
    profile z-statistics.}
  \item{maxsteps}{maximum number of points to be used for profiling each
    parameter.}
  \item{del}{suggested change on the scale of the profile
    t-statistics.  Default value chosen to allow profiling at about
    10 parameter values.}
  \item{trace}{logical: should the progress of profiling be reported?}
  \item{test}{profile Likelihood Ratio test or \I{Rao} Score test.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\value{
  A list of classes \code{"profile.glm"} and \code{"profile"} with an
  element for each parameter being profiled.  The elements are
  data-frames with two variables
  \item{par.vals}{a matrix of parameter values for each fitted model.}
  \item{tau or z}{the profile t or z-statistics (the name depends on whether  there is an estimated dispersion parameter.)}
}
\details{
  The profile z-statistic is defined either as (case \code{test = "LRT"}) the square root of change in
  deviance with an
  appropriate sign, or (case \code{test = "Rao"}) as the similarly
  signed square root of the \I{Rao} Score test
  statistic. The latter is defined as the squared gradient of the profile log likelihood
  divided by the profile Fisher information, but more conveniently
  calculated via the deviance of a Gaussian GLM fitted to the residuals
  of the profiled model.
}
\author{
  Originally, D. M. Bates and W. N. Venables.  (For S in 1996.)
}
\seealso{
  \code{\link{glm}}, \code{\link{profile}}, \code{\link{plot.profile}}
}
\examples{
options(contrasts = c("contr.treatment", "contr.poly"))
ldose <- rep(0:5, 2)
numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
sex <- factor(rep(c("M", "F"), c(6, 6)))
SF <- cbind(numdead, numalive = 20 - numdead)
budworm.lg <- glm(SF ~ sex*ldose, family = binomial)
pr1 <- profile(budworm.lg)
plot(pr1)
pairs(pr1)
}
\keyword{regression}
\keyword{models}
