\name{grep}
\title{Pattern Matching and Replacement}
\usage{
grep(pattern, x, ignore.case=FALSE, extended=TRUE, value=FALSE)
sub(pattern, replacement, x,
        ignore.case=FALSE, extended=TRUE)
gsub(pattern, replacement, x,
        ignore.case=FALSE, extended=TRUE)
}
\alias{grep}
\alias{sub}
\alias{gsub}
\arguments{
\item{pattern}{character string containing a regular expression
to be matched in the vector of character string \code{vec}.}
\item{x}{a vector of character strings where matches is sought.}
\item{ignore.case}{if \code{FALSE}, the pattern matching is
\emph{case sensitive} and if \code{TRUE}, case is ignored during matching.}
\item{extended}{if \code{TRUE}, extended regular expression matching
is used, and if \code{FALSE} basic regular expressions are used.}
\item{value}{if \code{FALSE}, a vector containing the indices
of the matches determined by \code{grep} is returned,
and if \code{TRUE}, a vector containing the matching
elements themselves is returned.}
\item{replacement}{a replacement for matched pattern in
\code{sub} and \code{gsub}.}
}
\description{
\code{grep} searches for matches to pattern given as its first
argument within the the vector of character strings given as
its second argument.

\code{sub} and \code{gsub} perform replacement of matches
determined by regular expression matching.
The functions differ only in that \code{sub} replaces only
the first occurence of a pattern whereas \code{gsub} replaces
all occurences.

The regular expressions used are those specified by POSIX 1003.2,
either extended or basic, depending on the value of the
\code{extended} argument.
}
\note{
This function is based on the \emph{regex} regular expression library
written by Henry Spencer of the University of Toronto.
}
\seealso{
\code{\link{match}}, \code{\link{pmatch}}.
}
\examples{
grep("[a-z]", letters)
}
\keyword{character}
\keyword{utilities}
