\name{readNEWS}
\alias{readNEWS}
\title{Read R's NEWS file or a Similar One}
\description{
  Read \R's NEWS file or a similarly formatted one.
  This is an experimental feature, new in R 2.4.0
  and may change in several ways.
}
\usage{
readNEWS(file = file.path(R.home(), "NEWS"), trace = FALSE,
         chop = c("first", "1", "par1", "keepAll"))
}
\arguments{
  \item{file}{the name of the file which the data are to be read from.
    Alternatively, \code{file} can be a \code{\link{connection}}, which
    will be opened if necessary, and can also be a complete URL.  For
    more details, see the \code{file} argument of \code{\link{read.table}}.}
  \item{trace}{logical indicating if the recursive reading should be
    traced, i.e., print what it is doing.}
  \item{chop}{a character string specifying how the news entries should
    be \emph{\dQuote{chopped}}; \code{chop = "keepAll"} saves the full
    entries.}
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
\value{
  An (S3) object of class \code{"newsTree"}; effectively
  a \code{\link{list}} of lists which is a tree of NEWS entries.

  Note that this is still experimental and may change in the future.
}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
NEWStr  <- readNEWS(trace = TRUE)# chop = "first" ( = "first non-empty")
## keep the full NEWS entry text i.e. "no chopping":
NEWStrA <- readNEWS(chop = "keepAll")
object.size(NEWStr)
object.size(NEWStrA) ## (no chopping) ==> about double the size

str(NEWStr, max = 3)

str(NEWStr[[c("2.3", "2.3.1")]], max=2, vec.len=1)

NEWStr [[c("2.3", "2.3.1", "NEW FEATURES")]]
NEWStrA[[c("2.4", "2.4.0", "NEW FEATURES")]]
}
\keyword{file}
\keyword{documentation}

