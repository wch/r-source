% File src/library/datasets/man/npk.Rd
% Part of the R package, http://www.R-project.org
% copyright (C) 1999 W. N. Venables and B. D. Ripley
% Distributed under GPL 2 or later

\name{npk}
\alias{npk}
\title{
  Classical N, P, K Factorial Experiment
}
\description{
  A classical N, P, K (nitrogen, phosphate, potassium) factorial
  experiment on the growth of peas conducted on 6 blocks. Each half of a
  fractional factorial design confounding the NPK interaction was used
  on 3 of the plots.
}
\usage{
npk
}
\format{
  The \code{npk} data frame has 24 rows and 5 columns:
  \describe{
    \item{\code{block}}{
      which block (label 1 to 6).
    }
    \item{\code{N}}{
      indicator (0/1) for the application of nitrogen.
    }
    \item{\code{P}}{
      indicator (0/1) for the application of phosphate.
    }
    \item{\code{K}}{
      indicator (0/1) for the application of potassium.
    }
    \item{\code{yield}}{
      Yield of peas, in pounds/plot (the plots were (1/70) acre).
    }
  }
}
\source{
  Imperial College, London, M.Sc. exercise sheet.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
% This gets different roundings
\examples{\donttest{
options(contrasts = c("contr.sum", "contr.poly"))
npk.aov <- aov(yield ~ block + N*P*K, npk)
npk.aov
summary(npk.aov)
coef(npk.aov)
options(contrasts = c("contr.treatment", "contr.poly"))
npk.aov1 <- aov(yield ~ block + N + K, data = npk)
summary.lm(npk.aov1)
se.contrast(npk.aov1, list(N=="0", N=="1"), data = npk)
model.tables(npk.aov1, type = "means", se = TRUE)
}}
\keyword{datasets}
