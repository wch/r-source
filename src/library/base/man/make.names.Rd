\name{make.names}
\alias{make.names}
\title{Make Legal R Names Out of Strings}
\usage{
make.names(names, unique=FALSE)
}
\arguments{
 \item{names}{character (vector) to be coerced to proper names.}
 \item{unique}{logical; if \code{TRUE}, the resulting strings are
     unique.  This may be desired for, e.g., column names.}
}
\description{
    Make legal \R names out of every \code{names[i]} string.
    Invalid characters are translated to \code{"."}.
}
\value{character vector of same length as \code{names} with each changed
    to a legal name.
}
\seealso{\code{\link{names}},\code{\link{character}},\code{\link{data.frame}}
}
\examples{
make.names(c("a and b", "a_and_b"), unique=TRUE)#-> "a.and.b"  "a.and.b1"

all(make.names(letters) == letters)# TRUE

data(state)
state.name[make.names(state.name) != state.name]# those 10 with a space
}
\keyword{character}
