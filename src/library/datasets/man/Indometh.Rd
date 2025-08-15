% File src/library/datasets/man/Indometh.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{Indometh}
\docType{data}
\alias{Indometh}
\title{Pharmacokinetics of Indomethacin}
\description{
  The \code{Indometh} data frame has 66 rows and 3 columns of data on
  the pharmacokinetics of indometacin (or, older spelling,
  \sQuote{indomethacin}).
}
\usage{Indometh}
\format{
  An object of class
  \code{c("nfnGroupedData", "nfGroupedData", "groupedData", "data.frame")}
  containing the following columns:
 \describe{
    \item{\code{Subject}}{
      an ordered factor with containing the subject codes.  The
      ordering is according to increasing maximum response.
    }
    \item{\code{time}}{
      a numeric vector of times at which blood samples were drawn (hr).
    }
    \item{\code{conc}}{
      a numeric vector of plasma concentrations of indometacin (\abbr{mcg}/ml).
    }
  }
}
\details{
  Each of the six subjects were given an intravenous injection of
  indometacin.

  This dataset was originally part of package \CRANpkg{nlme}, and that has
  methods (including for \code{[}, \code{as.data.frame}, \code{plot} and
  \code{print}) for its grouped-data classes.
}
\source{
  \bibcitet{R:Kwan+Breault+Umbenhauer:1976,
    |R:Davidian+Giltinan:1995|page 129,
    R:Pinheiro+Bates:2000}
}
\references{
  \bibshow{*}
}
\seealso{
  \code{\link{SSbiexp}} for models fitted to this dataset.
}
\keyword{datasets}
