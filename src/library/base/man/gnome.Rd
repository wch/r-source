\name{Gnome}
\alias{GNOME}
\alias{gnome}
\title{GNOME Desktop Graphics Device}
\usage{
gnome(display="", width=7, height=7, pointsize=12)
GNOME(display="", width=7, height=7, pointsize=12)
}
\arguments{
  \item{display}{the display on which the graphics window will appear.
    The default is to use the value in the user's environment variable
    \code{DISPLAY}.}
  \item{width}{the width of the plotting window in inches.}
  \item{height}{the height of the plotting window in inches.}
  \item{pointsize}{the default pointsize to be used.}
}
\description{
    \code{gnome} starts a \code{GNOME} compatible device driver.
    GNOME is an acronym for
    \bold{G}NU \bold{N}etwork \bold{O}bject \bold{M}odel \bold{E}nvironment.

%>> Lyndon, fill in the above; however it should only be a few lines
%>>    whereas the "details" should come below :
}
\details{
%%>>>    Please, Lyndon, fill this in ...
    This is still in development state.
}
\references{\url{www.gnome.org} and \url{www.gtk.org} for the
    \code{GTK+} (GIMP Tool Kit) libraries.
}
\author{Lyndon Drake \email{lyndon@stat.auckland.ac.nz}}
\note{The GNOME device is only available when explicitly desired at
    configure/compile time, see the toplevel \file{INSTALLATION} file.
}
\seealso{\code{\link{x11}},\code{\link{Devices}}.}
\examples{
\dontrun{
gnome(width=9)
}
}
\keyword{device}
