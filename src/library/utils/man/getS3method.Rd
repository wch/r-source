\name{getS3method}
\alias{getS3method}
\title{Get An S3 Method}
\description{
  Get a method for an S3 generic, possibly from a namespace. 
}
\usage{
getS3method(f, class, optional = FALSE)
}
\arguments{
  \item{f}{character: name of the generic.}
  \item{class}{character: name of the class.}
  \item{optional}{logical: should failure to find the generic or a
    method be allowed?}
}
\details{
  S3 methods may be hidden in packages with namespaces, and will not
  then be found by \code{\link{get}}: this function can retrieve
  such functions, primarily for debugging purposes.
}
\value{
  The function found, or \code{NULL} if no function is found and
  \code{optional = TRUE}.
}
\seealso{
  \code{\link{methods}}, \code{\link{get}}
}
\examples{
require(stats)
exists("predict.ppr") # false
getS3method("predict", "ppr")
}
\keyword{data}

