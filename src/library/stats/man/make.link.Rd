\name{make.link}
\alias{make.link}
\title{Create a Link for GLM families}
\usage{
make.link(link)
}
\arguments{
    \item{link}{character or numeric; one of \code{"logit"},
        \code{"probit"}, \code{"cloglog"}, \code{"identity"},
        \code{"log"},  \code{"sqrt"},  \code{"1/mu^2"},
        \code{"inverse"}, or number, say \eqn{\lambda}{lambda} resulting
        in power link \eqn{=\mu ^ \lambda}{mu ^ lambda}.}
}
%%-- implementation in ../R/family.R
\description{
    This function is used with the \code{\link{family}} functions in
    \code{\link{glm}()}.
    Given a link, it returns a link function, an inverse link
    function, the derivative \eqn{d\mu / d\eta}{dmu/deta} and a function
    for domain checking.
}
\value{
    A list with components
    \item{linkfun}{Link function \code{function(mu)}}
    \item{linkinv}{Inverse link function \code{function(eta)}}
    \item{mu.eta}{Derivative \code{function(eta)} \eqn{d\mu / d\eta}{dmu/deta}}
    \item{valideta}{\code{function(eta)}\{ \code{TRUE} if
        \code{eta} is in the domain of \code{linkinv} \}.}
}
\seealso{
  \code{\link{power}}, \code{\link{glm}}, \code{\link{family}}.
}
\examples{
str(make.link("logit"))

l2 <- make.link(2)
l2$linkfun(0:3)# 0 1 4 9
l2$mu.eta(eta= 1:2)#= 1/(2*sqrt(eta))
}
\keyword{models}

