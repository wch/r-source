% File src/library/grDevices/man/bringToTop.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{bringToTop}
\alias{bringToTop}
\alias{stayOnTop}

\title{Assign Focus to a Window}

\usage{
bringToTop(which = dev.cur(), stay = FALSE)
}

\arguments{
  \item{which}{a device number, or \code{-1}.}
  \item{stay}{whether to make the window stay on top.}
}

\description{
  \code{bringToTop} brings the specified screen device's window to the
  front of the window stack (and gives it focus).  With first argument
  \code{-1} it brings the console to the top.

  If \code{stay =  TRUE}, the window is designated as a topmost window,
  i.e.\sspace{}it will stay on top of any regular window.  \code{stay} may only be used
  when Rgui is run in SDI mode.  This corresponds to the \dQuote{Stay on top}
  popup menu item in Rgui.
}

\seealso{
  \code{\link{msgWindow}}, \code{\link{windows}}
}

\keyword{device}
