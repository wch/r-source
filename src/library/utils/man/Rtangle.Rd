\name{Rtangle}
\alias{Rtangle}
\alias{RtangleSetup}
\title{R Driver for Stangle}
\description{
  A driver for \code{\link{Stangle}} that extracts R code chunks.
}
\usage{
Rtangle()
RtangleSetup(file, syntax, output = NULL, annotate = TRUE,
             split = FALSE, prefix = TRUE, quiet = FALSE)
}
\arguments{
  \item{file}{Name of Sweave source file.}
  \item{syntax}{An object of class \code{SweaveSyntax}.}
  \item{output}{Name of output file, default is to remove extension \file{.nw},
    \file{.Rnw} or \file{.Snw} and to add extension \file{.R}. Any
    directory names in \code{file} are also removed such that the output
    is created in the current working directory.}
  \item{annotate}{By default, code chunks are separated by comment
    lines specifying the names and numbers of the code chunks. If
    \code{FALSE}, only the code chunks without any decorating comments
    are extracted.}
  \item{split}{Split output in single files per code chunk?}
  \item{prefix}{If \code{split = TRUE}, prefix the chunk labels by the
    basename of the input file to get output file names?}
  \item{quiet}{If \code{TRUE} all progress messages are suppressed.}
}
\author{Friedrich Leisch}
\references{
  Friedrich Leisch: Sweave User Manual, 2002\cr
  \url{http://www.ci.tuwien.ac.at/~leisch/Sweave}
}
\seealso{\code{\link{Sweave}}, \code{\link{RweaveLatex}}}
\keyword{utilities}

