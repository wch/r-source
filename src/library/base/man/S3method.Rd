\name{S3method}
\alias{.S3method}
\title{
  Register S3 Methods
}
\description{
  Register S3 methods in R scripts.
}
\usage{
.S3method(generic, class, method)
}
\arguments{
  \item{generic}{a character string naming an S3 generic function.}
  \item{class}{a character string naming an S3 class.}
  \item{method}{a character string or function giving the S3 method to
    be registered.  If not given, the function named
    \code{\var{generic}.\var{class}} is used.}
}
\details{
  This function should only be used in R scripts: for package code, one
  should use the correspoding \samp{S3method} \file{NAMESPACE} directive.
}
\examples{
## Create a generic function and register a method for objects
## inheriting from class 'cls':
gen <- function(x) UseMethod("gen")
met <- function(x) writeLines("Hello world.")
.S3method("gen", "cls", met)
## Create an object inheriting from class 'cls', and call the
## generic on it:
x <- structure(123, class = "cls")
gen(x)
}
