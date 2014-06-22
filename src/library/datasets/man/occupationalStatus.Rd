% File src/library/datasets/man/occupationalStatus.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2008 R Core Team
% Distributed under GPL 2 or later

\name{occupationalStatus}
\alias{occupationalStatus}
\docType{data}
\title{Occupational Status of Fathers and their Sons}
\description{
   Cross-classification of a sample of British males according to each
   subject's occupational status and his father's occupational status.
}
\usage{occupationalStatus}
\format{
   A \code{\link{table}} of counts, with classifying factors
   \code{origin} (father's occupational status; levels \code{1:8})
   and \code{destination} (son's occupational status; levels \code{1:8}).
}
\source{
  Goodman, L. A. (1979)
  Simple Models for the Analysis of Association in Cross-Classifications
  having Ordered Categories.
  \emph{J. Am. Stat. Assoc.}, \bold{74} (367), 537--552.

  The data set has been in package \CRANpkg{gnm} and been provided by the
  package authors.
}
\examples{
require(stats); require(graphics)

plot(occupationalStatus)

##  Fit a uniform association model separating diagonal effects
Diag <- as.factor(diag(1:8))
Rscore <- scale(as.numeric(row(occupationalStatus)), scale = FALSE)
Cscore <- scale(as.numeric(col(occupationalStatus)), scale = FALSE)
modUnif <- glm(Freq ~ origin + destination + Diag + Rscore:Cscore,
               family = poisson, data = occupationalStatus)

summary(modUnif)
plot(modUnif) # 4 plots, with warning about  h_ii ~= 1
}
\keyword{datasets}
