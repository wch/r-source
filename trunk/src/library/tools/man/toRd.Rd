% File src/library/tools/man/toRd.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2010-1 R Core Team
% Distributed under GPL 2 or later

\name{toRd}
\alias{toRd}
\alias{toRd.default}
\alias{toRd.bibentry}
\title{Generic Function to Convert Object to a Fragment of Rd Code}
\description{
Methods for this function render their associated classes as a fragment of Rd
code, which can then be rendered into text, HTML, or LaTeX.
}
\usage{
toRd(obj, ...)
\S3method{toRd}{bibentry}(obj, style = NULL, ...)
}
\arguments{
  \item{obj}{
The object to be rendered.
}
  \item{style}{
The style to be used in converting a \code{\link{bibentry}} object.
}
  \item{\dots}{
Additional arguments used by methods.
}
}
\details{
See \code{\link{bibstyle}} for a discussion of styles.  The default \code{style = NULL} value
gives the default style.
}
\value{
Returns a character vector containing a fragment of Rd code that could
be parsed and rendered.  The default method converts \code{obj} to mode \code{character},
then escapes any Rd markup within it.  The \code{bibentry} method converts an
object of that class to markup appropriate for use in a bibliography.
}
\keyword{ utilities }
\keyword{ documentation }
