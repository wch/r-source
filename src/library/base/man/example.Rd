\name{example}
\title{Run an Examples Section from the Online Help}
\usage{
example(topic, package = .packages(), lib,
        echo = TRUE, verbose = getOption("verbose"),
        prompt.echo = paste(abbreviate(topic, 6),"> ", sep=""))
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
}
\description{
  Run all the \R code from the \code{EXAMPLES} part of \R's online help
  topic \code{topic}.
}
\value{(the value of the last evaluated expression).}
\details{
  If \code{lib.loc} is not specified, the packages are searched for
  amongst those already loaded. If \code{lib.loc} is specified,
  they are searched for in the specified libraries, even if they are
  already loaded from another library. An attempt is made to load the
  package before running the examples, but this will not replace a
  pacakge loaded from another location.
}
\note{
  The examples can be many small files. On some file systems it is
  desirable to save space, and the files in the \file{R-ex} directory
  of an installed package can be zipped up as a zip archive \file{Rex.zip}.
}
\author{Martin Maechler and others}
\seealso{\code{\link{demo}}}.
\examples{
example("smooth", package="eda", lib.loc=.lib.loc)
example(dbinom)
}
\keyword{documentation}
\keyword{utilities}
