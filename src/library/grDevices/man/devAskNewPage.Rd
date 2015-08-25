% File src/library/grDevices/man/devAskNewPage.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2008 R Core Team
% Distributed under GPL 2 or later

\name{devAskNewPage}
\alias{devAskNewPage}
\title{ Prompt before New Page }
\description{
  This function can be used to control (for the current device)
  whether the user is prompted before starting a new page of output.
}
\usage{
devAskNewPage(ask = NULL)
}
\arguments{
  \item{ask}{\code{NULL} or a logical value.  If \code{TRUE}, the user
    will in future be prompted before a new page of output is started.}
}
\details{
  If the current device is the null device, this will open a graphics device.

  The default argument just returns the current setting and does not
  change it.

  The default value when a device is opened is taken from the
  setting of \code{\link{options}("device.ask.default")}.

  The precise circumstances when the user will be asked to confirm a new
  page depend on the graphics subsystem.  Obviously this needs to be an
  interactive session.  In addition \sQuote{recording} needs to be in
  operation, so only when the display list is enabled (see
  \code{\link{dev.control}}) which it usually is only on a screen device.
}
\value{
  The current prompt setting \emph{before} any new setting is applied.
  Invisibly if \code{ask} is logical.
}

\seealso{
  \code{\link{plot.new}}, \code{\link{grid.newpage}}
}
\keyword{ dplot }
