% File src/library/base/man/pcre_config.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2015-2017 R Core Team
% Distributed under GPL 2 or later

\name{pcre_config}
\alias{pcre_config}
\title{
  Report Configuration Options for PCRE
}
\description{
  Report some of the configuration options of the version of PCRE in use
  in this \R session.
}
\usage{
pcre_config()
}
\value{
  A named logical vector, currently with elements
  \item{UTF-8}{Support for UTF-8 inputs.  Required nowadays.}
  \item{Unicode properties}{Support for \samp{\p{xx}} and \samp{\P{xx}}
    in regular expressions.  Desirable and used by some CRAN packages.}
  \item{JIT}{Support for just-in-time compilation.  Only available as a
    compile-time option on certain architectures as from PCRE 8.20, and
    not used by \R itself prior to version 3.4.0.}
}
\seealso{
  \code{\link{extSoftVersion}} for the PCRE version.
}
\examples{
pcre_config()
}
