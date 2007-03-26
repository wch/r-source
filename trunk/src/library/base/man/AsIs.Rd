\name{AsIs}
\alias{I}
\alias{AsIs}
\alias{print.AsIs}
\alias{[.AsIs}
\title{Inhibit Interpretation/Conversion of Objects}
\description{
  Change the class of an object to indicate that it should be treated
  \dQuote{as is}.
}
\usage{
I(x)
}
\arguments{
  \item{x}{an object}
}
\details{
  Function \code{I} has two main uses.
  \itemize{
    \item In function \code{\link{data.frame}}.  Protecting an object by
    enclosing it in \code{I()} in a call to \code{data.frame} inhibits the
    conversion of character vectors to factors, and the dropping of
    names.  \code{I} can also be used to protect objects which are to be
    added to a data frame, or converted to a data frame \emph{via}
    \code{\link{as.data.frame}}.

    It achieves this by prepending the class \code{"AsIs"} to the object's
    classes.  Class \code{"AsIs"} has a few of its own methods, including
    for \code{[}, \code{as.data.frame}, \code{print} and \code{format}.

    \item In function \code{\link{formula}}.  There it is used to
    inhibit the interpretation of  operators such as \code{"+"},
    \code{"-"}, \code{"*"} and \code{"^"} as formula operators, so they
    are used as arithmetical operators.  This is interpreted as a symbol
    by \code{terms.formula}.
  }
}
\value{
  A copy of the object with class \code{"AsIs"} prepended to the class(es).
}
\references{
  Chambers, J. M. (1992)
  \emph{Linear models.}
  Chapter 4 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{data.frame}}, \code{\link{formula}}
}
\keyword{models}
