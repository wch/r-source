% File src/library/grDevices/man/grSoftVersion.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2014-2017 R Core Team
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
  A named character vector containing at least the elements
  
  \item{cairo}{the version of cairographics in use, or \code{""} if
    cairographics is not available.}

  It may also contain the versions of third-party software used by the
#ifdef windows
  standard
#endif
#ifdef unix
  X11-based
#endif
  bitmap devices:
  \item{libpng}{the version of \code{libpng} in use, or \code{""} if
    not available.}
  \item{jpeg}{the version of the JPEG headers used for compilation,
    or \code{""} if JPEG support was not compiled in.}
  \item{libtiff}{the version of \code{libtiff} in use, or \code{""} if
    not available.}

  It is conceivable but unlikely that the cairo-based bitmap devices
  will use different versions linked \emph{via} cairographics,
  especially \code{png(type = "cairo-png")}.
#ifdef unix
  If available on macOS, the Quartz-based devices will use the system
  versions of these libraries rather than those reported here.
#endif

  Unless otherwise stated the reported version is that of the (possibly
  dynamically-linked) library in use at runtime.

  Note that \code{libjpeg-turbo} as used on some Linux distributions
  reports its version as \code{"6.2"}, the IJG version from which it forked.
}

\seealso{
  \code{\link{extSoftVersion}} for versions of non-graphics software.
}
%% output is variable
\examples{
\donttest{grSoftVersion()}
}
