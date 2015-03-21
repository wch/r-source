% File src/library/utils/man/sourceutils.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2011-2013 R Core Team
% Distributed under GPL 2 or later

\name{sourceutils}
\alias{getSrcFilename}
\alias{getSrcDirectory}
\alias{getSrcref}
\alias{getSrcLocation}
\title{
  Source Reference Utilities
}
\description{
  These functions extract information from source references.
}
\usage{
getSrcFilename(x, full.names = FALSE, unique = TRUE)
getSrcDirectory(x, unique = TRUE)
getSrcref(x)
getSrcLocation(x, which = c("line", "column", "byte", "parse"),
               first = TRUE)
}
\arguments{
  \item{x}{
    An object (typically a function) containing source references.
  }
  \item{full.names}{
    Whether to include the full path in the filename result.
  }
  \item{unique}{
    Whether to list only unique filenames/directories.
  }
  \item{which}{
    Which part of a source reference to extract.  Can be abbreviated.
  }
  \item{first}{
    Whether to show the first (or last) location of the object.
  }
}
\details{
  Each statement of a function will have its own source reference if the 
  \code{"keep.source"} option is \code{TRUE}.  These functions retrieve
  all of them. 

  The components are as follows:
  \describe{
    \item{line}{The line number where the object starts or ends.}
    \item{column}{The column number where the object starts or ends.}
    \item{byte}{As for \code{"column"}, but counting bytes, which may
      differ in case of multibyte characters.}
    \item{parse}{As for \code{"line"}, but this ignores \code{#line} directives.}
  }
}
\value{
  \code{getSrcFilename} and \code{getSrcDirectory} return character vectors
  holding the filename/directory.

  \code{getSrcref} returns a list of \code{"srcref"} objects or
  \code{NULL} if there are none.

  \code{getSrcLocation} returns an integer vector of the requested type
  of locations.
}
\seealso{
\code{\link{srcref}}, \code{\link{getParseData}}
}
\examples{
fn <- function(x) {
  x + 1 # A comment, kept as part of the source
}			

# Show the temporary file directory
# where the example was saved

getSrcDirectory(fn)
getSrcLocation(fn, "line")
}
\keyword{ utilities }
