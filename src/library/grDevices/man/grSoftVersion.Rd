% File src/library/grDevices/man/grSoftVersion.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2014 R Core Team
% Distributed under GPL 2 or later

\name{grSoftVersion}
\alias{grSoftVersion}
\title{Report Versions of Graphics Software}
\description{
  Report versions of third-party graphics software.
}
\usage{
grSoftVersion()
}
\value{
  A named character vector containing at least the element
  
  \item{cairo}{the version of cairographics in use, or \code{""} if
    cairographics is not available.}

#ifdef windows
  On Windows it also contains the versions of third-party software used
  by the standard (not cairo-based) bitmap devices:
  \item{libpng}{the version of libpng in use, or \code{""} if
    not available.}
  \item{jpeg}{the version of jpeg headers used from compliation,
    or \code{""} if not compiled in.}
  \item{libtiff}{the version of libtiff in use, or \code{""} if
    not available.}
#endif
  
  Unless otherwise stated the reported version is that of the (possibly
  dynamically-linked) library in use at runtime.
}

\seealso{
  \code{\link{extSoftVersion}} for non-graphics software.
}
%% output is variable
\examples{
\donttest{grSoftVersion()}
}
