% File src/library/base/man/pcre_config.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2015 R Core Team
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
  \item{UTF-8}{Support for UTF-8 inputs.  Pretty much essential as
    all non-ASCII inputs are converted to UTF-8.}
  \item{Unicode properties}{Support for \samp{\p{xx}} and \samp{\P{xx}}
    in regular expressions.  Desirable.}
  \item{JIT}{Support for just-in-time compilation.  Desirable.}

  Early versions of the PCRE library may not have the last (it was
  added in 2011), in which case the report will be \code{NA}.
}
\seealso{
  \code{\link{extSoftVersion}} for the PCRE version.
}
\examples{
pcre_config()
}
