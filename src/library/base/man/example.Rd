\name{example}
\title{Run an Examples section from the online help}
\usage{
example(topic, package= .packages(), lib=.lib.loc,
        echo=TRUE, verbose=.Options$verbose, directory.sep="/")
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
 \item{directory.sep}{character; the directory separating character (an
     idea to make this OS independent).}
}
\description{
Run all the \R code from the \code{EXAMPLES} part of \R's online help
topic \code{topic}.
}
\value{(the value of the last evaluated expression).}
\author{Martin Maechler}
\seealso{\code{\link{demo}}}.
\examples{
example("smooth",package="eda")
}
\keyword{documentation}
\keyword{utilities}
