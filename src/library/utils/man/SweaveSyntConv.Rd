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
\references{
  Friedrich Leisch: Sweave User Manual, 2002\cr
  \url{http://www.ci.tuwien.ac.at/~leisch/Sweave}
}
\seealso{\code{\link{RweaveLatex}}, \code{\link{Rtangle}}}
\examples{
testfile <- system.file("Sweave", "Sweave-test-1.Rnw", package = "utils")

## convert the file to latex syntax
SweaveSyntConv(testfile, SweaveSyntaxLatex)

## and run it through Sweave
Sweave("Sweave-test-1.Stex")

\dontshow{if(!interactive()) unlink("Sweave-test-1*")}
}
\keyword{utilities}
