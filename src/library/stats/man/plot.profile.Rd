% File src/library/stats/man/plot.profile.Rd
% Part of the R package, https://www.R-project.org

% Originally:

% file MASS/man/plot.profile.Rd
% copyright (C) 1999-2023 W. N. Venables and B. D. Ripley
%

% Edits by Peter Dalgaard 2023, (C) R Core Team
%
% - call stats:::profile in example

% FIXME: really need a glm() example in here


\name{plot.profile}
\alias{plot.profile}
\alias{pairs.profile}
\title{Plotting Functions for 'profile' Objects}
\description{
  \code{\link{plot}} and \code{\link{pairs}} methods for objects of
  class \code{"profile"}.
}
\usage{
\method{plot}{profile}(x, ...)
\method{pairs}{profile}(x, colours = 2:3, which = names(x), ...)
}
\arguments{
  \item{x}{an object inheriting from class \code{"profile"}.}
  \item{colours}{colours to be used for the mean curves conditional on
    \code{x} and \code{y} respectively.}
  \item{which}{names or number of parameters in pairs plot}
  \item{\dots}{arguments passed to or from other methods.}
}
\details{
  This is the main \code{plot} method for objects created by
  \code{\link{profile.glm}}.  It can also be called on objects created
  by \code{\link{profile.nls}}, but they have a specific method,
  \code{\link{plot.profile.nls}}.

  The \code{pairs} method shows, for each pair of parameters x and
  y, two curves intersecting at the maximum likelihood estimate, which
  give the loci of the points at which the tangents to the contours of
  the bivariate profile likelihood become vertical and horizontal,
  respectively.  In the case of an exactly bivariate normal profile
  likelihood, these two curves would be straight lines giving the
  conditional means of y|x and x|y, and the contours would be exactly
  elliptical. The \code{which} argument allows you to select a subset
  of parameters; the default corresponds to the set of parameters that have 
  been profiled. 
}
\author{
  Originally, D. M. Bates and W. N. Venables for S (in 1996).
  Taken from \pkg{MASS} where these functions were re-written by
  B. D. Ripley for \R (by 1998).
}
\seealso{
  \code{\link{profile.glm}}, \code{\link{profile.nls}}.
}
\examples{
## see ?profile.glm for another example using glm fits.

## a version of example(profile.nls) from R >= 2.8.0
fm1 <- nls(demand ~ SSasympOrig(Time, A, lrc), data = BOD)
pr1 <- profile(fm1, alphamax = 0.1)
stats:::plot.profile(pr1) ## override dispatch to plot.profile.nls
pairs(pr1) # a little odd since the parameters are highly correlated

## an example from ?nls
x <- -(1:100)/10
y <- 100 + 10 * exp(x / 2) + rnorm(x)/10
nlmod <- nls(y ~  Const + A * exp(B * x), start=list(Const=100, A=10, B=1))
pairs(profile(nlmod))

## example from Dobson (1990) (see ?glm)
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
## this example is only formally a Poisson model. It is really a 
## comparison of 3 multinomials. Only the interaction parameters are of 
## interest.
glm.D93i <- glm(counts ~ outcome * treatment, family = poisson())
pr1 <- profile(glm.D93i)
pr2 <- profile(glm.D93i, which=6:9)
plot(pr1)
plot(pr2)
pairs(pr1)
pairs(pr2)
}

\keyword{models}
\keyword{hplot}
