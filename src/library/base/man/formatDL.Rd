% File src/library/base/man/formatDL.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{formatDL}
\alias{formatDL}
\title{Format Description Lists}
\description{Format vectors of items and their descriptions as 2-column
  tables or LaTeX-style description lists.
}
\usage{
formatDL(x, y, style = c("table", "list"),
         width = 0.9 * getOption("width"), indent = NULL)
}
\arguments{
  \item{x}{a vector giving the items to be described, or a list of
    length 2 or a matrix with 2 columns giving both items and
    descriptions.}
  \item{y}{a vector of the same length as \code{x} with the
    corresponding descriptions.  Only used if \code{x} does not already
    give the descriptions.}
  \item{style}{a character string specifying the rendering style of the
    description information.  Can be abbreviated.
    If \code{"table"}, a two-column table with
    items and descriptions as columns is produced (similar to Texinfo's
    \verb{@table} environment.  If \code{"list"}, a LaTeX-style tagged
    description list is obtained.}
  \item{width}{a positive integer giving the target column for wrapping
    lines in the output.}
  \item{indent}{a positive integer specifying the indentation of the
    second column in table style, and the indentation of continuation
    lines in list style.  Must not be greater than \code{width/2}, and
    defaults to \code{width/3} for table style and \code{width/9} for
    list style.}
}
\value{
  a character vector with the formatted entries.
}
\details{
  After extracting the vectors of items and corresponding descriptions
  from the arguments, both are coerced to character vectors.

  In table style, items with more than \code{indent - 3} characters are
  displayed on a line of their own.
}
\examples{
## Provide a nice summary of the numerical characteristics of the
## machine R is running on:
writeLines(formatDL(unlist(.Machine)))
## Inspect Sys.getenv() results in "list" style (by default, these are
## printed in "table" style):
writeLines(formatDL(Sys.getenv(), style = "list"))
}
\keyword{print}
