\name{pretty}
\title{Pretty Breakpoints}
\usage{
pretty(x, n=5, shrink.sml = 2^-3))
}
\alias{pretty}
\arguments{
\item{x}{numeric vector}
\item{n}{integer giving the \emph{desired} number of values.}
\item{shrink.sml}{numeric factor, positive (ideally a small power of 2),
  by a which a default scale is shrunk, in the case when
  \code{range(x)} is ``very small''.}
}
\value{
  a numeric sequence of about \code{n} equally spaced nice values which
  cover the range of the values in \code{x}.
  The values are chosen so that they are 1, 2 or 5 times a power of 10.
}
\examples{
pretty(1:15)
pretty(1:15 * 2)
pretty(1:20)
pretty(1:20, n=2)
pretty(1:20, n=10)

##-- more bizarre, using ``shrink.sml'':
pretty(2)
pretty(1.234e100)
pretty(pi)
pretty(1001.1001)
pretty(1001.1001, shrink = 1)
for(k in 5:11) {cat("k=",k,": "); print(pretty(101 + c(0, pi*10^-k)))}
for(k in -7:3)
  cat("shrink=",formatC(2^k,wid=9),":",
      formatC(pretty(1001.1001, shrink = 2^k), wid=6),"\n")
}
\keyword{dplot}
