\name{gl}
\title{Generate Factor Levels}
\usage{
gl(n, k, length = n*k, labels=1:n, ordered=FALSE)
}
\alias{gl}
\value{
This function generates \code{\link{factor}}s by specifying the
pattern of their levels.
The result has levels from \code{1} to \code{n}
with each value replicated in groups of length
\code{k} out to a total length of \code{length}.
Labels for the resulting factor levels can be
optionally specified with the arguments \code{labels}
and the factor levels can be made ordered by
specifying \code{ordered=TRUE}.
\code{gl} is modelled on the \emph{GLIM} function of the
same name.
}
\seealso{the underlying \code{\link{factor}(.)}.}
\examples{
# First control, then treatment:
gl(2,8, label=c("Ctnrl","Treat"))
# 20 alternating 1s and 2s
gl(2, 1, 20)
# alternating pairs of 1s and 2s
gl(2, 2, 20)

}
\keyword{category}
\keyword{arith}
