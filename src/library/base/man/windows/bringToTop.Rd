\name{bringToTop}
\alias{bringToTop}

\title{Assign Focus to a Window}

\usage{
bringToTop(which = dev.cur())
}

\arguments{
  \item{which}{a device number, or \code{-1}.}
}

\description{
  \code{bringToTop} brings the specified screen device's window to the
  front of the window stack (and gives it focus).  With argument
  \code{-1}, it brings the console to the top.
}

\seealso{
  \code{\link{windows}}
}

\keyword{device}
