% File src/library/datasets/man/Harman74.cor.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{Harman74.cor}
\docType{data}
\alias{Harman74.cor}
\title{Harman Example 7.4}
\description{
  A correlation matrix of 24 psychological tests given to 145 seventh and
  eight-grade children in a Chicago suburb by Holzinger and Swineford.
}
\usage{Harman74.cor}
\source{
  Harman, H. H. (1976)
  \emph{Modern Factor Analysis}, Third Edition Revised,
  University of Chicago Press, Table 7.4.
}
\examples{
require(stats)
(Harman74.FA <- factanal(factors = 1, covmat = Harman74.cor))
for(factors in 2:5) print(update(Harman74.FA, factors = factors))
Harman74.FA <- factanal(factors = 5, covmat = Harman74.cor,
                        rotation = "promax")
print(Harman74.FA$loadings, sort = TRUE)
}
\keyword{datasets}
