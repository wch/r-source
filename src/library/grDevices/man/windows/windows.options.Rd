% File src/library/grDevices/man/pdf.options.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{windows.options}
\alias{windows.options}
\title{Auxiliary Function to Set/View Defaults for Arguments of windows()}
\description{
  The auxiliary function \code{windows.options} can be used to set or view
  (if called without arguments) the default values for the arguments of
  \code{\link{windows}}.

  \code{windows.options} needs to be called before calling \code{windows},
  and the default values it sets can be overridden by supplying
  arguments to \code{windows}.
  }
\usage{
windows.options(\dots, reset = FALSE)
}
\arguments{
  \item{\dots}{arguments \code{width}, \code{height}, \code{pointsize},
    \code{record}, \code{rescale}, \code{xpinch}, \code{ypinch},
    \code{bg}, \code{canvas}, \code{gamma}, \code{xpos}, \code{ypos},
    \code{buffered}, \code{restoreConsole}, \code{clickToConfirm},
    \code{title}, \code{fillOddEven} and \code{antialias} can be supplied.}
  \item{reset}{logical: should the defaults be reset to their
    \sQuote{factory-fresh} values?}
}
\details{
  If both \code{reset = TRUE} and \code{...} are supplied the defaults
  are first reset to the \sQuote{factory-fresh} values and then the new
  values are applied.

  Option \code{antialias} applies to screen devices (\code{windows},
  \code{win.graph}, \code{X11} and \code{x11}).  There is a separate option,
  \code{bitmap.aa.win}, for bitmap devices with \code{type = "windows"}.
}
\value{
  A named list of all the defaults.  If any arguments are supplied the
  returned values are the old values and the result has the visibility
  flag turned off.
}
\seealso{
  \code{\link{windows}}, \code{\link{ps.options}}.
}
\examples{\dontrun{
## put something like this is your .Rprofile to customize the defaults
setHook(packageEvent("grDevices", "onLoad"),
        function(...)
            grDevices::windows.options(width = 8, height = 6,
                                       xpos = 0, pointsize = 10,
                                       bitmap.aa.win = "cleartype"))
}}
\keyword{device}
