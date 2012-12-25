% File src/library/base/man/strwrap.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{strwrap}
\alias{strwrap}
\title{Wrap Character Strings to Format Paragraphs}
\description{
  Each character string in the input is first split into paragraphs (or
  lines containing whitespace only).  The paragraphs are then formatted
  by breaking lines at word boundaries.  The target columns for wrapping
  lines and the indentation of the first and all subsequent lines of a
  paragraph can be controlled independently.
}
\usage{
strwrap(x, width = 0.9 * getOption("width"), indent = 0,
        exdent = 0, prefix = "", simplify = TRUE, initial = prefix)
}
\arguments{
  \item{x}{a character vector, or an object which can be converted to a
    character vector by \code{\link{as.character}}.}
  \item{width}{a positive integer giving the target column for wrapping
    lines in the output.}
  \item{indent}{a non-negative integer giving the indentation of the
    first line in a paragraph.}
  \item{exdent}{a non-negative integer specifying the indentation of
    subsequent lines in paragraphs.}
  \item{prefix, initial}{a character string to be used as prefix for
    each line except the first, for which \code{initial} is used.}
  \item{simplify}{a logical.  If \code{TRUE}, the result is a single
    character vector of line text; otherwise, it is a list of the same
    length as \code{x} the elements of which are character vectors of
    line text obtained from the corresponding element of \code{x}.
    (Hence, the result in the former case is obtained by unlisting that
    of the latter.)}
}
\details{
  Whitespace (space, tab or newline characters) in the input is
  destroyed.  Double spaces after periods, question and explanation
  marks (thought as representing sentence ends) are preserved.
  Currently, possible sentence ends at line breaks are not considered
  specially.

  Indentation is relative to the number of characters in the prefix
  string.
}
\value{
  A character vector in the current locale's encoding (if
  \code{simplify} is \code{TRUE}), or a list of such character vectors.
}
\examples{
## Read in file 'THANKS'.
x <- paste(readLines(file.path(R.home("doc"), "THANKS")), collapse = "\n")
## Split into paragraphs and remove the first three ones
x <- unlist(strsplit(x, "\n[ \t\n]*\n"))[-(1:3)]
## Join the rest
x <- paste(x, collapse = "\n\n")
## Now for some fun:
writeLines(strwrap(x, width = 60))
writeLines(strwrap(x, width = 60, indent = 5))
writeLines(strwrap(x, width = 60, exdent = 5))
writeLines(strwrap(x, prefix = "THANKS> "))

## Note that messages are wrapped AT the target column indicated by
## 'width' (and not beyond it).
## From an R-devel posting by J. Hosking <jh910@juno.com>.
x <- paste(sapply(sample(10, 100, replace = TRUE),
           function(x) substring("aaaaaaaaaa", 1, x)), collapse = " ")
sapply(10:40,
       function(m)
       c(target = m, actual = max(nchar(strwrap(x, m)))))
}
\keyword{character}
