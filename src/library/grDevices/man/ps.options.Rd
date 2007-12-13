% File src/library/grDevices/man/ps.options.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{ps.options}
\alias{ps.options}
\title{Auxiliary Function to Set/View Defaults for Arguments of postscript}
\description{
  The auxiliary function \code{ps.options} can be used to set or view
  (if called without arguments) the default values for some of the
  arguments to \code{\link{postscript}}.
  
  \code{ps.options} needs to be called before calling \code{postscript},
  and the default values it sets can be overridden by supplying
  arguments to \code{postscript}.
  }
\usage{
ps.options(\dots, reset = FALSE, override.check = FALSE)
}
\arguments{
  \item{\dots}{arguments \code{onefile}, \code{family}, \code{title},
    \code{fonts},
    \code{encoding}, \code{bg}, \code{fg}, \code{width}, \code{height},
    \code{horizontal}, \code{pointsize},  \code{paper},
    \code{pagecentre}, \code{print.it}, \code{command} and
    \code{colormodel} can be supplied.}
  \item{reset}{logical: should the defaults be reset to their defaults?}
  \item{override.check}{logical argument passed to
    \code{\link{check.options}}.  See the Examples.}
}
\details{
  For backwards compatibility argument \code{append} is
  accepted but ignored with a warning.
}
\value{
  A named list of all the defaults.  If \code{\dots} or
  \code{reset = TRUE} is supplied these are the old values and the
  result has the visibility flag turned off.
}
\seealso{
  \code{\link{postscript}}, \code{\link{pdf.options}}
}
\examples{
ps.options(bg = "pink")
utils::str(ps.options())

### ---- error checking of arguments: ----
ps.options(width=0:12, onefile=0, bg=pi)
# override the check for 'width', but not 'bg':
utils::str(ps.options(width=0:12, bg=pi, override.check = c(TRUE,FALSE)))
ps.options(reset = TRUE) # back to factory-fresh
}
\keyword{device}
