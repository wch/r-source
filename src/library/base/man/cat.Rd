\name{cat}
\title{Concatenate and Print}
\usage{
cat(\dots , file = "", sep = " ", fill = FALSE, labels = NULL,
    append = FALSE)
}
\alias{cat}
\arguments{
  \item{\dots}{}
  \item{file}{character string naming the file to print to.  If
    \code{""}, \code{cat} prints to the standard output.}
  \item{sep}{character string to insert between the objects to print.}
  \item{fill}{???}
  \item{labels}{character vector of labels for the lines printed.}
  \item{append}{if \code{TRUE}, output will be appended to \code{file};
    otherwise, it will overwrite the contents of \code{file}.}
}
\value{
  \code{cat} converts its arguments to character strings, concatenates
  them, separating them by the given \code{sep=} string, and then prints
  them.

  \code{cat} is useful for producing output in user defined functions.
}
\examples{
## print an informative message
cat("iteration = ", iter <- iter + 1, "\n")
}
\keyword{print}
\keyword{file}
