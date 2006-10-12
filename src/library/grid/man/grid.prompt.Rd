\name{grid.prompt}
\alias{grid.prompt}
\title{ Prompt before new page }
\description{
  This function can be used to control whether the user
  is prompted before starting a new page of output.
}
\usage{
grid.prompt(ask)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ask}{ a logical value.  If \code{TRUE}, the user
    is prompted before a new page of output is started. }
}
\value{
  The current prompt setting \emph{before} any new setting is applied.
}
\author{ Paul Murrell }
\seealso{ \code{\link{grid.newpage}} }
\keyword{ dplot }% at least one, from doc/KEYWORDS
