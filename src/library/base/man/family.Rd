\name{family}
\title{Family Objects for Models}
\usage{
family(object)

binomial(link=logit)
gaussian()
Gamma(link=inverse)
inverse.gaussian()
poisson(link=log)
quasi(link=identity, variance=constant)
}
\alias{family}
\alias{binomial}
\alias{gaussian}
\alias{Gamma}
\alias{inverse.gaussian}
\alias{poisson}
\alias{quasi}
\arguments{
\item{link}{a specification for the model link function.
The \code{binomial} family admits the links; \code{logit},
\code{probit} and \code{cloglog} (complementary log-log),
the \code{Gamma} family the links; \code{identity},
\code{inverse} and \code{log},
the \code{poisson} family the links; \code{identity}
\code{log} and \code{sqrt}, and
the \code{quasi} family the links; \code{logit},
\code{probit}, \code{cloglog},  \code{identity}, \code{inverse},
\code{log}, \code{1/mu^2} and \code{sqrt}.
The other families have only a single permissible
link function.  These are the \code{identity} for the \code{gaussian}
family and \code{1/mu^2} for the \code{inverse.gaussian} family.
The function \code{power} can also be used to create a
power link function for the \code{quasi} family.}
\item{variance}{for all families, other than \code{quasi},
the variance function is determined by the family.
The \code{quasi} family will accept the specifications
\code{constant}, \code{mu(1-mu)}, \code{mu}, \code{mu^2} and
\code{mu^3} as variance function.}
\item{object}{the function \code{family} accesses the
\code{family} objects which are stored within objects
created by modelling functions (e.g. \code{glm}).}
}
\description{
Family objects provide a convenient way to specify
the details of the models used by functions such as \code{glm}.
See the documentation for \code{glm} for the details on
how such model fitting takes place.
}
\references{
McCullagh P. and J. A. Nelder (1989).
\emph{Generalized Linear Models}.
London: Chapman and Hall.

Dobson, A. J. (1983).
\emph{An Introduction to Statistical Modelling}.
London: Chapman and Hall.

Cox, D. R. and E. J. Snell (1981).
\emph{Applied Statistics; Principles and Examples}.
London: Chapman and Hall.
}
\seealso{
\code{\link{glm}}, \code{\link{power}}.
}
\keyword{models}
