% File src/library/stats/man/make.link.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{make.link}
\alias{make.link}
\title{Create a Link for GLM Families}
%%-- implementation in ../R/family.R
\description{
  This function is used with the \code{\link{family}} functions in
  \code{\link{glm}()}.
  Given the name of a link, it returns a link function, an inverse link
  function, the derivative \eqn{d\mu / d\eta}{dmu/deta} and a function
  for domain checking.
}
\usage{
make.link(link)
}
\arguments{
  \item{link}{character; one of \code{"logit"},
    \code{"probit"}, \code{"cauchit"}, \code{"cloglog"}, \code{"identity"},
    \code{"log"},  \code{"sqrt"},  \code{"1/mu^2"}, \code{"inverse"}.
  }
}
\value{
  A object of class \code{"link-glm"}, a list with components
  \item{linkfun}{Link function \code{function(mu)}}
  \item{linkinv}{Inverse link function \code{function(eta)}}
  \item{mu.eta}{Derivative \code{function(eta)} \eqn{d\mu / d\eta}{dmu/deta}}
  \item{valideta}{\code{function(eta)}\{ \code{TRUE} if
    \code{eta} is in the domain of \code{linkinv} \}.}
  \item{name}{a name to be used for the link}.
}
\seealso{
  \code{\link{power}}, \code{\link{glm}}, \code{\link{family}}.
}
\examples{
utils::str(make.link("logit"))
}
\keyword{models}

