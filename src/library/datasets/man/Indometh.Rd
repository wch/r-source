\name{Indometh}
\docType{data}
\alias{Indometh}
\title{Pharmacokinetics of Indomethicin}
\description{
  The \code{Indometh} data frame has 66 rows and 3 columns of data on
  the pharmacokinetics of indomethicin.
}
\usage{Indometh}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Subject}{
      an ordered factor with containing the subject codes.  The
      ordering is according to increasing maximum response.
    }
    \item{time}{
      a numeric vector of times at which blood samples were drawn (hr).
    }
    \item{conc}{
      a numeric vector of plasma concentrations of indomethicin (mcg/ml).
    }
  }
}
\details{
  Each of the six subjects were given an intravenous injection of
  indomethicin.
}
\source{
  Kwan, Breault, Umbenhauer, McMahon and Duggan (1976),
  \dQuote{Kinetics of Indomethicin absorption, elimination, and
    enterohepatic circulation in man},
  \emph{Journal of Pharmacokinetics and Biopharmaceutics}, \bold{4},
  255--280.

  Davidian, M. and Giltinan, D. M. (1995) \emph{Nonlinear Models for
    Repeated Measurement Data}, Chapman & Hall (section 5.2.4, p. 134)

  Pinheiro, J. C. and Bates, D. M. (2000) \emph{Mixed-effects Models in
    S and S-PLUS}, Springer.
}
\examples{
require(stats)
fm1 <- nls(conc ~ SSbiexp(time, A1, lrc1, A2, lrc2),
           data = Indometh, subset = Subject == 1)
summary(fm1)
}
\keyword{datasets}
