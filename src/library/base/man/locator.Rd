\name{locator}
\title{Graphical Input}
\usage{
locator(n=512)
}
\alias{locator}
\description{
\code{locator} reads the position of the graphics cursor when
the (first) mouse button is pressed.  Unless the process
is terminated prematurely by the user (see below) at most
\code{n} positions are determined.

The input process can be terminated prematurely by
pressing any mouse button other than the first,
or by clicking outside the graphics window.
}
\value{
A list containing \code{x} and \code{y} components which are
the coordinates of the identified points.
}
\keyword{iplot}
