% File src/library/tools/man/readNEWS.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Distributed under GPL 2 or later

\name{readNEWS}
\alias{readNEWS}
\alias{checkNEWS}
\title{Read R's NEWS file or a similar one}
\description{
  Read \R's NEWS file or a similarly formatted one.
  This is an experimental feature, new in R 2.4.0
  and may change in several ways.
}
\usage{
readNEWS(file = file.path(R.home(), "NEWS"), trace = FALSE,
         chop = c("first", "1", "par1", "keepAll"))
checkNEWS(file = file.path(R.home(), "NEWS"))
}
\arguments{
  \item{file}{the name of the file which the data are to be read from.
    Alternatively, \code{file} can be a \code{\link{connection}}, which
    will be opened if necessary, and can also be a complete URL.  For
    more details, see the \code{file} argument of \code{\link{read.table}}.}
  \item{trace}{logical indicating if the recursive reading should be
    traced, i.e., print what it is doing.}
  \item{chop}{a character string specifying how the news entries should
    be \emph{chopped}; \code{chop = "keepAll"} saves the full entries.}
}
\details{
\code{readNEWS()} reads a NEWS file; \code{checkNEWS()} checks for common
errors in formatting.  Currently it detects an incorrect number of spaces before
the \code{"o"} item marker.
}
\value{
  \code{readNEWS()} returns an (S3) object of class \code{"newsTree"}; effectively
  a \code{\link{list}} of lists which is a tree of NEWS entries.

 \code{checkNEWS()} returns \code{TRUE} if no suspected errors are found,
 or prints a message for each suspected error and returns \code{FALSE}.
 
  Note that this is still experimental and may change in the future.
}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
NEWStr  <- readNEWS(trace = TRUE)# chop = "first" ( = "first non-empty")
## keep the full NEWS entry text i.e. "no chopping":
NEWStrA <- readNEWS(chop = "keepAll")
object.size(NEWStr)
object.size(NEWStrA) ## (no chopping) ==> about double the size

str(NEWStr, max.level = 3)

str(NEWStr[[c("2.3", "2.3.1")]], max.level=2, vec.len=1)

NEWStr [[c("2.3", "2.3.1", "NEW FEATURES")]]
NEWStrA[[c("2.4", "2.4.0", "NEW FEATURES")]]

# Check the current NEWS file

stopifnot(checkNEWS())

}
\keyword{file}
\keyword{documentation}

