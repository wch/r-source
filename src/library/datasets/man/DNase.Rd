\name{DNase}
\docType{data}
\alias{DNase}
\title{Elisa assay of DNase}
\description{
  The \code{DNase} data frame has 176 rows and 3 columns of data
  obtained during development of an ELISA assay for the recombinant
  protein DNase in rat serum.
}
\usage{DNase}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Run}{
      an ordered factor with levels \code{10} < \dots < \code{3}
      indicating the assay run.
    }
    \item{conc}{
      a numeric vector giving the known concentration of the
      protein. 
    }
    \item{density}{
      a numeric vector giving the measured optical density
      (dimensionless) in the assay.  Duplicate optical density
      measurements were obtained. 
    }
  }
}
\source{
  Davidian, M. and Giltinan, D. M. (1995) \emph{Nonlinear Models for
    Repeated Measurement Data}, Chapman & Hall (section 5.2.4, p. 134)

  Pinheiro, J. C. and Bates, D. M. (2000) \emph{Mixed-effects Models in
    S and S-PLUS}, Springer.
}
\examples{
require(stats)
coplot(density ~ conc | Run, data = DNase,
    show = FALSE, type = "b")
coplot(density ~ log(conc) | Run, data = DNase,
    show = FALSE, type = "b")
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
