\name{family}
\title{Family Objects for Models}
\usage{
family(object)

binomial(link = "logit")
gaussian(link ="identity")
Gamma(link = "inverse")
inverse.gaussian(link = "1/mu^2")
poisson(link = "log")
quasi(link = "identity", variance = "constant")

print.family(x, \dots)
}
\alias{family}
\alias{binomial}
\alias{gaussian}
\alias{Gamma}
\alias{inverse.gaussian}
\alias{poisson}
\alias{quasi}
\alias{print.family}
\arguments{
  \item{link}{a specification for the model link function.
    The \code{binomial} family admits the links \code{"logit"},
    \code{"probit"}, \code{"log"}, and \code{"cloglog"} (complementary
    log-log);
    the \code{Gamma} family the links \code{"identity"},
    \code{"inverse"}, and \code{"log"};
    the \code{poisson} family the links \code{"identity"}, \code{"log"},
    and \code{"sqrt"};
    the \code{quasi} family the links \code{"logit"}, \code{"probit"},
    \code{"cloglog"},  \code{"identity"}, \code{"inverse"},
    \code{"log"}, \code{"1/mu^2"} and \code{"sqrt"}.
    The function \code{\link{power}} can also be used to create a
    power link function for the \code{quasi} family.

    The other families have only one permissible link function:
    \code{"identity"} for the \code{gaussian} family, and
    \code{"1/mu^2"} for the \code{inverse.gaussian} family.}
  \item{variance}{for all families, other than \code{quasi}, the
    variance function is determined by the family.  The \code{quasi}
    family will accept the specifications \code{"constant"},
    \code{"mu(1-mu)"}, \code{"mu"}, \code{"mu^2"} and \code{"mu^3"} for
    the variance function.}
  \item{object}{the function \code{family} accesses the \code{family}
    objects which are stored within objects created by modelling
    functions (e.g. \code{glm}).}
}
\description{
  Family objects provide a convenient way to specify the details of the
  models used by functions such as \code{\link{glm}}.  See the
  documentation for \code{\link{glm}} for the details on how such model
  fitting takes place.
}
\references{
  McCullagh P. and Nelder, J. A. (1989)
  \emph{Generalized Linear Models.}
  London: Chapman and Hall.

  Dobson, A. J. (1983)
  \emph{An Introduction to Statistical Modelling.}
  London: Chapman and Hall.

  Cox, D. R. and  Snell, E. J. (1981).
  \emph{Applied Statistics; Principles and Examples.}
  London: Chapman and Hall.
}
\seealso{
  \code{\link{glm}}, \code{\link{power}}.
}
\examples{
nf <- gaussian()# Normal family
nf
str(nf)# internal STRucture

gf <- Gamma()
gf
str(gf)
gf$linkinv
all(1:10 == gf$linkfun(gf$linkinv(1:10)))# is TRUE
gf$variance(-3:4) #- == (.)^2

## tests of quasi
x <- rnorm(100)
y <- rpois(100, exp(1+x))
glm(y ~x, family=quasi(var="mu", link="log"))
# which is the same as
glm(y ~x, family=poisson)
glm(y ~x, family=quasi(var="mu^2", link="log"))
\dontrun{glm(y ~x, family=quasi(var="mu^3", link="log")) # should fail}
y <- rbinom(100, 1, plogis(x))
# needs to set a starting value for the next fit
glm(y ~x, family=quasi(var="mu(1-mu)", link="logit"), start=c(0,1))
}
\keyword{models}
