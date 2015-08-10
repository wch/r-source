% File src/library/utils/man/windows/setInternet2.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{setInternet2}
\alias{setInternet2}

\title{
Set or disable the use of Windows internal functions for Internet access.
}
\description{
  This used to control whether \code{method = "internal"} mapped to
  \code{method = "wininet"} in functions \code{\link{download.file}} and
  \code{\link{url}}.

  As from \R 3.3.0 it changes nothing, and only \code{use = NA} is accepted.
}

\usage{
setInternet2(use = TRUE)
}
\arguments{
  \item{use}{logical: whether to use the Windows internet functions.  If
    \code{NA}, just return the current setting.}
}
\value{
  For backwards compatibility, \code{TRUE}.
}
\author{
  Duncan Murdoch
}
\examples{
setInternet2(NA)
}
\keyword{ utilities }
