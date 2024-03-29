% File src/library/datasets/man/esoph.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{esoph}
\docType{data}
\alias{esoph}
\title{Smoking, Alcohol and (O)esophageal Cancer}
\description{
  Data from a case-control study of (o)esophageal cancer in
  \I{Ille-et-Vilaine}, France.
}
\usage{esoph}
\format{
  A data frame with records for 88 age/alcohol/tobacco combinations.

  \tabular{rlll}{
    [,1] \tab \code{agegp} \tab Age group \tab 1  25--34 years\cr
    \tab \tab \tab 2  35--44\cr
    \tab \tab \tab 3  45--54\cr
    \tab \tab \tab 4  55--64\cr
    \tab \tab \tab 5  65--74\cr
    \tab \tab \tab 6  75+\cr
    [,2] \tab \code{alcgp} \tab Alcohol consumption \tab 1   0--39 gm/day\cr
    \tab \tab \tab 2  40--79\cr
    \tab \tab \tab 3  80--119\cr
    \tab \tab \tab 4  120+\cr
    [,3] \tab \code{tobgp} \tab Tobacco consumption \tab 1   0-- 9 gm/day\cr
    \tab \tab \tab 2  10--19\cr
    \tab \tab \tab 3  20--29\cr
    \tab \tab \tab 4  30+\cr
    [,4] \tab \code{ncases} \tab Number of cases \tab \cr
    [,5] \tab \code{ncontrols} \tab Number of controls \tab
  }
}
\source{
  Breslow, N. E. and Day, N. E. (1980)
  \emph{Statistical Methods in Cancer Research. Volume 1: The Analysis of
    Case-Control Studies.}  IARC Lyon / Oxford University Press.
  % Currently at
  % <https://publications.iarc.fr/Book-And-Report-Series/Iarc-Scientific-Publications/Statistical-Methods-In-Cancer-Research-Volume-I-The-Analysis-Of-Case-Control-Studies-1980>.
}
\author{Thomas Lumley}
\examples{
require(stats)
require(graphics) # for mosaicplot
summary(esoph)
## effects of alcohol, tobacco and interaction, age-adjusted
model1 <- glm(cbind(ncases, ncontrols) ~ agegp + tobgp * alcgp,
              data = esoph, family = binomial())
anova(model1)
## Try a linear effect of alcohol and tobacco
model2 <- glm(cbind(ncases, ncontrols) ~ agegp + unclass(tobgp)
                                         + unclass(alcgp),
              data = esoph, family = binomial())
summary(model2)
## Re-arrange data for a mosaic plot
ttt <- table(esoph$agegp, esoph$alcgp, esoph$tobgp)
o <- with(esoph, order(tobgp, alcgp, agegp))
ttt[ttt == 1] <- esoph$ncases[o]
tt1 <- table(esoph$agegp, esoph$alcgp, esoph$tobgp)
tt1[tt1 == 1] <- esoph$ncontrols[o]
tt <- array(c(ttt, tt1), c(dim(ttt),2),
            c(dimnames(ttt), list(c("Cancer", "control"))))
mosaicplot(tt, main = "esoph data set", color = TRUE)
}
\keyword{datasets}
