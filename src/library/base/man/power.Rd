\name{power}
\alias{power}
\title{Create a Power Link Object}
\usage{
power(lambda = 1)
}
\arguments{
    \item{lambda}{a real number.}
}
\description{
    Creates a link object based on the link function \eqn{\eta = \mu ^
	\lambda}{eta = mu^lambda}.
}
\details{
    If \code{lambda} is non-negative, it is taken as zero, and the log
    link is obtained.  The default \code{lambda = 1} gives the identity
    link.
}
\value{
    A list with components \code{linkfun}, \code{linkinv},
    \code{mu.eta}, and \code{valideta}.
    See \code{\link{make.link}} for information on their meaning.
}
\seealso{
    \code{\link{make.link}},
    \code{\link{family}}
}
\examples{
power()
quasi(link=power(1/3))[c("linkfun", "linkinv")]
}
\keyword{models}
