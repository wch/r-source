\name{methods}
\title{List Methods for S3 Generic Functions or Classes}
\alias{methods}
\alias{print.MethodsFunction}
\description{
  List all available methods for an S3 generic function, or all
  methods for a class.
}
\usage{
methods(generic.function, class)
}
\arguments{
  \item{generic.function}{a generic function, or a character string naming a
    generic function.}
  \item{class}{a symbol or character string naming a class: only used if
    \code{generic.function} is not supplied.}
}
\value{
  An object of class \code{"MethodsFunction"}, a
  character vector of function names with an \code{"info"} attribute.
  There is a \code{print} method which marks with an asterisk any
  methods which are not visible: such functions can be examined by
  \code{\link{getS3method}} or \code{\link{getAnywhere}}.

  The \code{"info"} attribute is a data frame, currently with a
  logical column, \code{visible} and a factor column \code{from}
  (indicating where the methods were found).
}
\details{
  Function \code{methods} can be used to find out about the methods for
  a particular generic function or class.  The functions listed are those
  which \emph{are named like methods} and may not actually be methods
  (known exceptions are discarded in the code).  Note that the listed
  methods may not be user-visible objects, but often help will be
  available for them.

  If \code{class} is used, we check that a matching generic can be found
  for each user-visible object named.  If \code{generic.function} is
  given, there is a warning if it appears not to be a generic function.
  (The check for being generic used can be fooled.)
}
\note{
  This scheme is called \emph{S3} (S version 3).  For new projects,
  it is recommended to use the more flexible and robust \emph{S4} scheme
  provided in the \pkg{methods} package.  Functions can have both S3
  and S4 methods, and function \code{\link{showMethods}} will
  list the S4 methods (possibly none).

  The original \code{methods} function was written by Martin Maechler.
}
\seealso{
  \code{\link{S3Methods}}, \code{\link[base]{class}}, \code{\link{getS3method}}
}
\references{
  Chambers, J. M. (1992)
  \emph{Classes and methods: object-oriented programming in S.}
  Appendix A of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth \& Brooks/Cole.
}
\examples{
methods(summary)
methods(class = "aov")
methods("[[")    # uses C-internal dispatching
methods("$")
methods("$<-")   # replacement function
methods("+")     # binary operator
methods("Math")  # group generic
methods("axis")  # looks like it has methods, but not generic
\dontrun{methods(print)   # over 100
}}
\keyword{methods}
