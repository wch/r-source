\name{formatDL}
\alias{formatDL}
\title{Format Description Lists}
\description{}
\usage{
formatDL(x, y, style = c("table", "list"),
         width = 0.9 * getOption("width"), indent = NULL)
}
\arguments{
  \item{x}{a vector giving the items to be described.  Will be coerced
    to a character vector.}
  \item{y}{a vector of the same length as \code{x} with the
    corresponding descriptions.  Will be coerced to a character vector.}
  \item{style}{a character string specifying the rendering style of the
    description information.  If \code{"table"}, a two-column table with
    items and descriptions as columns is produced (similar to Texinfo's
    \code{@table} environment.  If \code{"list"}, a LaTeX-style tagged
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
  In table style, items with more than \code{indent - 3} characters are
  displayed on a line of their own.
}
\examples{
## Use R to create the `INDEX' for package `eda' from its `CONTENTS'
x <- read.dcf(file = system.file("CONTENTS", package = "eda"),
               fields = c("Entry", "Description"))
x <- as.data.frame(x)
writeLines(formatDL(x$Entry, x$Description))
## Same information in tagged description list style:
writeLines(formatDL(x$Entry, x$Description, style = "list"))
}
\keyword{print}

