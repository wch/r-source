% File src/library/utils/man/SweaveSyntConv.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{SweaveSyntConv}
\alias{SweaveSyntConv}
\title{Convert Sweave Syntax}
\description{
  This function converts the syntax of files in \code{\link{Sweave}}
  format to another Sweave syntax definition.
}
\usage{
SweaveSyntConv(file, syntax, output = NULL)
}
\arguments{
  \item{file}{Name of Sweave source file.}
  \item{syntax}{An object of class \code{SweaveSyntax} or a character
    string with its name giving the target syntax to which the file is
    converted.}
  \item{output}{Name of output file, default is to remove the extension
    from the input file and to add the default extension of the target
    syntax. Any directory names in \code{file} are also removed such
    that the output is created in the current working directory.}}
\author{Friedrich Leisch}
\seealso{
  \sQuote{\href{../doc/Sweave.pdf}{Sweave User Manual}}, a vignette in
  the \pkg{utils} package.

  \code{\link{RweaveLatex}}, \code{\link{Rtangle}}
}
\examples{
testfile <- system.file("Sweave", "Sweave-test-1.Rnw", package = "utils")
\dontshow{oldwd <- setwd(tempdir())}

## convert the file to latex syntax
SweaveSyntConv(testfile, SweaveSyntaxLatex)

## and run it through Sweave
Sweave("Sweave-test-1.Stex")

\dontshow{ if(!interactive()) unlink("Sweave-test-1*") ; setwd(oldwd) }
}
\keyword{utilities}
