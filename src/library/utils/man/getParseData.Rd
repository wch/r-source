% File src/library/utils/man/getParseData.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2012-2014 R Core Team
% Distributed under GPL 2 or later

\name{getParseData}
\alias{getParseData}
\alias{getParseText}
\title{
Get detailed parse information from object.
}
\description{
  If the \code{"keep.source"} option is \code{TRUE}, \R's parser
  will attach detailed information on the object it has parsed.  These
  functions retrieve that information.
}
\usage{
getParseData(x, includeText = NA)
getParseText(parseData, id)
}
\arguments{
  \item{x}{
    an expression returned from \code{\link{parse}}, or a function or other
    object with source reference information
  }
  \item{includeText}{
    logical; whether to include the text of parsed items in the result
  }
  \item{parseData}{
    a data frame returned from \code{getParseData}
  }
  \item{id}{
    a vector of item identifiers whose text is to be retrieved
  }
}
\details{
  In version 3.0.0, the \R{} parser was modified to include code written
  by Romain Francois in his \pkg{parser} package.  This constructs a
  detailed table of information about every token and higher level
  construct in parsed code.  This table is stored in the
  \code{\link{srcfile}} record associated with source references in the
  parsed code, and retrieved by the \code{getParseData} function.
}
\value{
  For \code{getParseData}:\cr
  If parse data is not present, \code{NULL}.  Otherwise
  a data frame is returned, containing the following columns:
  \item{line1}{integer. The line number where the item starts.  This is the
    parsed line number called \code{"parse"} in \code{\link{getSrcLocation}},
    which ignores \verb{#line} directives.}
  \item{col1}{integer. The column number where the item starts.  The first character
    is column 1.  This corresponds to \code{"column"} in \code{\link{getSrcLocation}}.}
  \item{line2}{integer. The line number where the item ends.}
  \item{col2}{integer. The column number where the item ends.}
  \item{id}{integer. An identifier associated with this item.}
  \item{parent}{integer. The \code{id} of the parent of this item.}
  \item{token}{character string. The type of the token.}
  \item{terminal}{logical.  Whether the token is \dQuote{terminal}, i.e.
    a leaf in the parse tree.}
  \item{text}{character string. If \code{includeText} is \code{TRUE}, the
    text of all tokens; if it is \code{NA} (the default), the text of terminal
    tokens.  If \code{includeText == FALSE}, this column is not included.}

  The rownames of the data frame will be equal to the \code{id} values,
  and the data frame will have a \code{"srcfile"} attribute containing
  the \code{\link{srcfile}} record which was used.  The rows will be
  ordered by starting position within the source file, with parent items
  occurring before their children.

  For \code{getParseText}:\cr
  A character vector of the same length as \code{id} containing the associated
  text items.  If they are not included in \code{parseData}, they will be
  retrieved from the original file.
}
\references{
  Romain Francois (2012). parser: Detailed R source code parser. R
  package version 0.0-16. \url{https://github.com/halpo/parser}.
}
\author{
Duncan Murdoch
}
\note{
  There are a number of differences in the results returned by
  \code{getParseData} relative to those in the original \pkg{parser}
  code:
  \itemize{
    \item Fewer columns are kept.
    \item The internal token number is not returned.
    \item \code{col1} starts counting at 1, not 0.
    \item The \code{id} values are not attached to the elements of the parse
    tree, they are only retained in the table returned by \code{getParseData}.
    \item \verb{#line} directives are identified, but other comment
    markup (e.g., \CRANpkg{roxygen} comments) are not.
  }
}

\seealso{
\code{\link{parse}}, \code{\link{srcref}}
}
\examples{
fn <- function(x) {
  x + 1 # A comment, kept as part of the source
}

d <- getParseData(fn)
if (!is.null(d)) {
  plus <- which(d$token == "'+'")
  sum <- d$parent[plus]
  print(d[as.character(sum),])
  print(getParseText(d, sum))
}
}
\keyword{  utilities  }
