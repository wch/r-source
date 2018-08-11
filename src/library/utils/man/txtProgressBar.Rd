% File src/library/utils/man/txtProgressBar.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2008-11, 18 R Core Team
% Distributed under GPL 2 or later

\name{txtProgressBar}
\alias{txtProgressBar}
\alias{getTxtProgressBar}
\alias{setTxtProgressBar}
\alias{close.txtProgressBar}
\title{Text Progress Bar}
\description{
  Text progress bar in the \R console.
}
\usage{
txtProgressBar(min = 0, max = 1, initial = 0, char = "=",
               width = NA, title, label, style = 1, file = "")

getTxtProgressBar(pb)
setTxtProgressBar(pb, value, title = NULL, label = NULL)
\method{close}{txtProgressBar}(con, \dots)
}
\arguments{
  \item{min, max}{(finite) numeric values for the extremes of the
    progress bar. Must have \code{min < max}.}
  \item{initial, value}{initial or new value for the progress bar.  See
    \sQuote{Details} for what happens with invalid values.}
  \item{char}{the character (or character string) to form the progress bar.}
  \item{width}{the width of the progress bar, as a multiple of the width
    of \code{char}.  If \code{NA}, the default, the number of characters
    is that which fits into \code{getOption("width")}.}
  \item{style}{the \sQuote{style} of the bar -- see \sQuote{Details}.}
  \item{file}{an open connection object or \code{""} which indicates
    the console: \code{\link{stderr}()} might be useful here.}
  \item{pb, con}{an object of class \code{"txtProgressBar"}.}
  \item{title, label}{ignored, for compatibility with other progress bars.}
  \item{\dots}{for consistency with the generic.}
}
\details{
  \code{txtProgressBar} will display a progress bar on the \R console
  (or a connection) via a text representation.

  \code{setTxtProgessBar} will update the value.  Missing
  (\code{\link{NA}}) and out-of-range values of \code{value} will be
  (silently) ignored.  (Such values of \code{initial} cause the progress
  bar not to be displayed until a valid value is set.)

  The progress bar should be \code{close}d when finished with: this
  outputs the final newline character.

  \code{style = 1} and \code{style = 2} just shows a line of
  \code{char}. They differ in that \code{style = 2} redraws the line
  each time, which is useful if other code might be writing to the \R
  console.  \code{style = 3} marks the end of the range by \code{|} and
  gives a percentage to the right of the bar.
}
\value{
  For \code{txtProgressBar} an object of class \code{"txtProgressBar"}.

  For \code{getTxtProgressBar} and \code{setTxtProgressBar}, a
  length-one numeric vector giving the previous value (invisibly for
  \code{setTxtProgressBar}).
}
\note{
  Using \code{style} 2 or 3 or reducing the value with \code{style = 1}
  uses \samp{\r} to return to the left margin -- the interpretation of
  carriage return is up to the terminal or console in which \R is
  running, and this is liable to produce ugly output on a connection
  other than a terminal, including when \code{stdout()} is
  redirected to a file.
}
\seealso{
  \code{\link{winProgressBar}} (Windows only),
  \code{\link{tkProgressBar}} (Unix-alike platforms).
}
\examples{\donttest{ # slow
testit <- function(x = sort(runif(20)), ...)
{
    pb <- txtProgressBar(...)
    for(i in c(0, x, 1)) {Sys.sleep(0.5); setTxtProgressBar(pb, i)}
    Sys.sleep(1)
    close(pb)
}
testit()
testit(runif(10))
testit(style = 3)
}}
\keyword{utilities}
