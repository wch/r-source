\name{make.names}
\alias{make.names}
\title{Make Syntactically Valid Names Out of Strings}
\description{
  Make syntactically valid names out of character strings.
}
\usage{
make.names(names, unique = FALSE)
}
\arguments{
 \item{names}{character (vector) to be coerced to syntactically valid
   names.}
 \item{unique}{logical; if \code{TRUE}, the resulting strings are
     unique.  This may be desired for, e.g., column names.}
}
\value{
  A character vector of same length as \code{names} with each changed to
  a syntactically valid name.
}
\details{
  A syntactically valid name consists of letters, numbers, and the dot
  character and starts with a letter or the dot.

  All invalid characters are translated to \code{"."}.
}
\seealso{
  \code{\link{names}},
  \code{\link{character}},
  \code{\link{data.frame}}.
}
\examples{
make.names(c("a and b", "a_and_b"), unique=TRUE)#-> "a.and.b"  "a.and.b1"

all(make.names(letters) == letters)# TRUE

data(state)
state.name[make.names(state.name) != state.name]# those 10 with a space
}
\keyword{character}
