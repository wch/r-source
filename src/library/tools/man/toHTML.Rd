% File src/library/tools/man/toHTML.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2010-1 R Core Team
% Distributed under GPL 2 or later

\name{toHTML}
\alias{toHTML}
\alias{toHTML.packageIQR}
\alias{toHTML.news_db}
\title{
Display an object in HTML.
}
\description{
This generic function generates a complete HTML page from an object.
}
\usage{
toHTML(x, ...)
\S3method{toHTML}{packageIQR}(x, ...)
\S3method{toHTML}{news_db}(x, ...)
}
\arguments{
  \item{x}{ An object to display. }
  \item{\dots}{ Optional parameters for methods; the \code{"packageIQR"} and
  \code{"news_db"} methods pass these to \code{\link{HTMLheader}}. }
}
\value{
A character vector to display the object \code{x}.  The \code{"packageIQR"} method
is designed to display lists in the \R{} help system.
}
\seealso{
\code{\link{HTMLheader}}
}
\examples{
cat(toHTML(demo(package = "base")), sep = "\n")
}
\keyword{ utilities }
\keyword{ documentation }
