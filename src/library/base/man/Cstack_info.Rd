\name{Cstack_info}
\alias{Cstack_info}
\title{Report Information on C Stack Size and Usage}
\description{
  Report information on the C stack size and usage (if available).
}
\usage{
Cstack_info()
}
\details{
  On most platforms, C stack information is recorded when \R is
  initialized and used for stack-checking.  If this information is
  unavailable, the \code{size} will be returned as \code{-1}, and
  stack-checking is not performed.

  The information on the base address is thought to be accurate on
  Windows, Linux and FreeBSD, but a heuristic is used on other
  platforms.  Because this might be slightly inaccurate, the current
  usage could be estimated as negative.
}
\value{
  A \emph{double} vector (to allow 64-bit addresses to be returned,
  possibly inaccurately). This has named elements
  \item{base}{The base address of the stack.}
  \item{size}{The size of the stack (in bytes), or \code{-1} if unknown.}
  \item{directioon}{\code{1} (stack grows down) or \code{-1} (stack grows up).}
  \item{current}{The estimated current usage (in bytes).}
}
\examples{
Cstack_info()
}
\keyword{ utilities }

