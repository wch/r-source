% File src/library/grDevices/man/ps.options.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2008 R Core Team
% Distributed under GPL 2 or later

\name{ps.options}
\alias{ps.options}
\alias{setEPS}
\alias{setPS}
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

setEPS(\dots)
setPS(\dots)
}
\arguments{
  \item{\dots}{arguments \code{onefile}, \code{family}, \code{title},
    \code{fonts},
    \code{encoding}, \code{bg}, \code{fg}, \code{width}, \code{height},
    \code{horizontal}, \code{pointsize},  \code{paper},
    \code{pagecentre}, \code{print.it}, \code{command},
    \code{colormodel} and \code{fillOddEven} can be supplied.  \code{onefile},
    \code{horizontal} and \code{paper} are \emph{ignored}
    for \code{setEPS} and \code{setPS}.}
  \item{reset}{logical: should the defaults be reset to their
    \sQuote{factory-fresh} values?}
  \item{override.check}{logical argument passed to
    \code{\link{check.options}}.  See the Examples.}
}
\details{
  If both \code{reset = TRUE} and \code{...} are supplied the defaults
  are first reset to the \sQuote{factory-fresh} values and then the new
  values are applied.

  For backwards compatibility argument \code{append} is accepted but
  ignored with a warning.

  \code{setEPS} and \code{setPS} are wrappers to set defaults
  appropriate for figures for inclusion in documents (the default size
  is 7 inches square unless \code{width} or \code{height} is supplied)
  and for spooling to a PostScript printer respectively.  For historical
  reasons the latter is the ultimate default.
}
\value{
  A named list of all the previous defaults.  If \code{\dots} or
  \code{reset = TRUE} is supplied the result has the visibility flag
  turned off.
}
\seealso{
  \code{\link{postscript}}, \code{\link{pdf.options}}
}
\examples{
ps.options(bg = "pink")
utils::str(ps.options())

### ---- error checking of arguments: ----
ps.options(width = 0:12, onefile = 0, bg = pi)
# override the check for 'width', but not 'bg':
ps.options(width = 0:12, bg = pi, override.check = c(TRUE,FALSE))
utils::str(ps.options())
ps.options(reset = TRUE) # back to factory-fresh
}
\keyword{device}
