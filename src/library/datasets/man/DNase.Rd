% File src/library/datasets/man/DNase.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{DNase}
\docType{data}
\alias{DNase}
\title{Elisa assay of \abbr{DNase}}
\description{
  The \code{DNase} data frame has 176 rows and 3 columns of data
  obtained during development of an ELISA assay for the recombinant
  protein \abbr{DNase} in rat serum.
}
\usage{DNase}
\format{
  An object of class
  \code{c("nfnGroupedData", "nfGroupedData", "groupedData", "data.frame")}
  containing the following columns:
  \describe{
    \item{\code{Run}}{
      an ordered factor with levels \code{10} < \dots < \code{3}
      indicating the assay run.
    }
    \item{\code{conc}}{
      a numeric vector giving the known concentration of the
      protein.
    }
    \item{\code{density}}{
      a numeric vector giving the measured optical density
      (dimensionless) in the assay.  Duplicate optical density
      measurements were obtained.
    }
  }
}
\details{
  This dataset was originally part of package \CRANpkg{nlme}, and that has
  methods (including for \code{[}, \code{as.data.frame}, \code{plot} and
  \code{print}) for its grouped-data classes.
}
\source{
  \bibinfo{R:Davidian+Giltinan:1995}{footer}{(Section 5.2.4, page 134.)}
  \bibshow{R:Davidian+Giltinan:1995,
    R:Pinheiro+Bates:2000}
}
\examples{
require(stats); require(graphics)
\dontshow{options(show.nls.convergence=FALSE)}
coplot(density ~ conc | Run, data = DNase,
       show.given = FALSE, type = "b")
coplot(density ~ log(conc) | Run, data = DNase,
       show.given = FALSE, type = "b")
## fit a representative run
fm1 <- nls(density ~ SSlogis( log(conc), Asym, xmid, scal ),
    data = DNase, subset = Run == 1)
## compare with a four-parameter logistic
fm2 <- nls(density ~ SSfpl( log(conc), A, B, xmid, scal ),
    data = DNase, subset = Run == 1)
summary(fm2)
anova(fm1, fm2)
}
\keyword{datasets}
