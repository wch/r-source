\name{missing}
\title{Does a Formal Argument have a Value?}
\usage{missing(x)}
\alias{missing}
\description{
\code{missing} can be used to test whether a value was specified
as an argument to a function.
The following example shows how a plotting function
can be written to work with either a pair of vectors
giving x and y coordinates of points to be plotted
or a single vector giving y values to be plotted
against their indexes.
}
\seealso{\code{\link{substitute}} for argument expression;
    \code{\link{NA}} for ``missing values'' in data.}
\examples{
myplot <- function(x,y) {
                if(missing(y)) {
                        y <- x
                        x <- 1:length(y)
                }
                plot(x,y)
        }
}
\keyword{programming}
