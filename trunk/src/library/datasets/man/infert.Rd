\name{infert}
\docType{data}
\alias{infert}
\title{Infertility after Spontaneous and Induced Abortion}
\description{
  This is a matched case-control study dating from before the
  availability of conditional logistic regression.
}
\usage{infert}
\format{
  \tabular{rll}{
    1.  \tab Education  \tab 0 = 0-5  years \cr
        \tab            \tab 1 = 6-11 years \cr
        \tab            \tab 2 = 12+  years  \cr
    2.  \tab age        \tab age in years of case \cr
    3.  \tab parity     \tab count \cr
    4.  \tab number of prior \tab 0 = 0 \cr
        \tab induced abortions \tab 1 = 1 \cr
        \tab            \tab 2 = 2 or more \cr
    5.  \tab case status\tab 1 = case \cr
        \tab            \tab 0 = control \cr
    6.  \tab number of prior \tab 0 = 0 \cr
        \tab spontaneous abortions \tab 1 = 1 \cr
        \tab            \tab 2 = 2 or more \cr
    7.  \tab matched set number \tab 1-83 \cr
    8.  \tab stratum number \tab 1-63}
}
\source{
  Trichopoulos et al. (1976)
  \emph{Br. J. of Obst. and Gynaec.} \bold{83}, 645--650.
}
\note{
  One case with two prior spontaneous abortions and two prior induced
  abortions is omitted.
}
\examples{
require(stats)
model1 <- glm(case ~ spontaneous+induced, data=infert,family=binomial())
summary(model1)
## adjusted for other potential confounders:
summary(model2 <- glm(case ~ age+parity+education+spontaneous+induced,
                data=infert,family=binomial()))
## Really should be analysed by conditional logistic regression
## which is in the survival package
if(require(survival)){
  model3 <- clogit(case~spontaneous+induced+strata(stratum),data=infert)
  print(summary(model3))
  detach()# survival (conflicts)
}
}
\keyword{datasets}
