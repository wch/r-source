\name{example}
\title{Run an Examples Section from the Online Help}
\usage{
example(topic, package= .packages(), lib=.lib.loc,
        echo=TRUE, verbose=.Options$verbose,
        prompt.echo = paste(abbreviate(topic, 6),"> ", sep=""),
        directory.sep= "/")
}
\alias{example}
\arguments{
 \item{topic}{name or character:  The online \code{\link{help}} topic
     the examples of which should be run.}
 \item{package}{a character vector with package names.}
 \item{lib}{a character vector with path names of \R package libraries.}
 \item{echo}{logical;  if \code{TRUE}, show the \R input when sourcing.}
 \item{verbose}{logical;  if \code{TRUE}, show even more when running
     example code.}
 \item{prompt.echo}{character; gives the prompt to be used if
     \code{echo = TRUE}.}
 \item{directory.sep}{character; the directory separating character (an
     idea to make this OS independent).}
}
\description{
Run all the \R code from the \code{EXAMPLES} part of \R's online help
topic \code{topic}.
}
\value{(the value of the last evaluated expression).}
\note{
    The examples can be many small files. On some file systems it is
    desirable to save space, and the files in the \file{R-ex} directory
    of an installed package can be zipped up as a zip archive \file{Rex.zip}.
}
\author{Martin Maechler}
\seealso{\code{\link{demo}}}.
\examples{
example("smooth", package="eda")
example(dbinom)
}
\keyword{documentation}
\keyword{utilities}
