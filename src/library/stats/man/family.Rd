\name{family}
\title{Family Objects for Models}
\usage{
family(object, \dots)

binomial(link = "logit")
gaussian(link = "identity")
Gamma(link = "inverse")
inverse.gaussian(link = "1/mu^2")
poisson(link = "log")
quasi(link = "identity", variance = "constant")
quasibinomial(link = "logit")
quasipoisson(link = "log")
}
\alias{family}
\alias{binomial}
\alias{gaussian}
\alias{Gamma}
\alias{inverse.gaussian}
\alias{poisson}
\alias{quasi}
\alias{quasibinomial}
\alias{quasipoisson}
\alias{print.family}
\arguments{
  \item{link}{a specification for the model link function.
    The \code{gaussian} family accepts the links \code{"identity"},
    \code{"log"} and \code{"inverse"};
    the \code{binomial} family the links \code{"logit"},
    \code{"probit"}, \code{"log"} and \code{"cloglog"} (complementary
    log-log);
    the \code{Gamma} family the links \code{"inverse"}, \code{"identity"}
     and \code{"log"};
    the \code{poisson} family the links \code{"log"}, \code{"identity"}, 
    and \code{"sqrt"} and the \code{inverse.gaussian} family the links
    \code{"1/mu^2"}, \code{"inverse"}, \code{"identity"} and \code{"log"}.
    
    The \code{quasi} family allows the links \code{"logit"}, \code{"probit"},
    \code{"cloglog"},  \code{"identity"}, \code{"inverse"},
    \code{"log"}, \code{"1/mu^2"} and \code{"sqrt"}.
    The function \code{\link{power}} can also be used to create a
    power link function for the \code{quasi} family.
  }
  \item{variance}{for all families, other than \code{quasi}, the
    variance function is determined by the family.  The \code{quasi}
    family will accept the specifications \code{"constant"},
    \code{"mu(1-mu)"}, \code{"mu"}, \code{"mu^2"} and \code{"mu^3"} for
    the variance function.}
  \item{object}{the function \code{family} accesses the \code{family}
    objects which are stored within objects created by modelling
    functions (e.g., \code{glm}).}
  \item{\dots}{further arguments passed to methods.}
}
\description{
  Family objects provide a convenient way to specify the details of the
  models used by functions such as \code{\link{glm}}.  See the
  documentation for \code{\link{glm}} for the details on how such model
  fitting takes place.
}
\details{
  The \code{quasibinomial} and \code{quasipoisson} families differ from
  the \code{binomial} and \code{poisson} families only in that the
  dispersion parameter is not fixed at one, so they can \dQuote{model}
  over-dispersion.  For the binomial case see McCullagh and Nelder
  (1989, pp. 124--8).  Although they show that there is (under some
  restrictions) a model with
  variance proportional to mean as in the quasi-binomial model, note
  that \code{glm} does not compute maximum-likelihood estimates in that
  model.  The behaviour of S is closer to the quasi- variants. 
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

  Hastie, T. J. and Pregibon, D. (1992)
  \emph{Generalized linear models.}
  Chapter 6 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth \& Brooks/Cole.
}
\author{
  The design was inspired by S functions of the same names described
  in Hastie \& Pregibon (1992).
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
gf$variance(-3:4) #- == (.)^2

## quasipoisson. compare with example(glm)
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
d.AD <- data.frame(treatment, outcome, counts)
glm.qD93 <- glm(counts ~ outcome + treatment, family=quasipoisson())
glm.qD93
anova(glm.qD93, test="F")
summary(glm.qD93)
## for Poisson results use
anova(glm.qD93, dispersion = 1, test="Chisq")
summary(glm.qD93, dispersion = 1)


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
