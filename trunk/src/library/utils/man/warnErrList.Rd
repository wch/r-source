\name{warnErrList}
\alias{warnErrList}
\title{Collect and Summarize Errors From List}
\description{
  Collect errors (class \code{"error"}, typically from \code{\link{tryCatch}})
  from a list \code{x} into a \dQuote{summary warning}, by default
  produce a \code{\link{warning}} and keep that message as
  \code{"warningMsg"} attribute.
}
\usage{
warnErrList(x, warn = TRUE, errValue = NULL)
}
\arguments{
  \item{x}{a \code{\link{list}}, typically from applying models to a
    list of data (sub)sets, e.g., using \code{\link{tryCatch}(*, error = identity)}.
  }
  \item{warn}{logical indicating if \code{\link{warning}()} should be
    called.}
  \item{errValue}{the value with which errors should be replaced.}
}
\value{
  a \code{\link{list}} of the same length and names as the \code{x}
  argument, with the error components replaced by \code{errValue},
  \code{\link{NULL}} by default, and summarized in the
  \code{"warningMsg"} attribute.
}
\seealso{
  The \code{warnErrList()} utility has been used in
  \code{\link[nlme]{lmList}()} and \code{\link[nlme]{nlsList}()} in
  recommended package \CRANpkg{nlme} forever.
}
\examples{
## Regression for each Chick:
ChWtgrps <- split(ChickWeight, ChickWeight[,"Chick"])
sapply(ChWtgrps, nrow)# typically 12 obs.
nlis1 <- lapply(ChWtgrps, function(DAT) tryCatch(error = identity,
                      lm(weight ~ (Time + I(Time^2)) * Diet, data = DAT)))
nl1 <- warnErrList(nlis1) #-> warning :
## 50 times the same error (as Diet has only one level in each group)
stopifnot(sapply(nl1, is.null)) ## all errors --> all replaced by NULL
nlis2 <- lapply(ChWtgrps, function(DAT) tryCatch(error = identity,
                      lm(weight ~ Time + I(Time^2), data = DAT)))
nl2 <- warnErrList(nlis2)
stopifnot(identical(nl2, nlis2)) # because there was *no* error at all
nlis3 <- lapply(ChWtgrps, function(DAT) tryCatch(error = identity,
                      lm(weight ~ poly(Time, 3), data = DAT)))
nl3 <- warnErrList(nlis3) # 1 error caught:
stopifnot(inherits(nlis3[[1]], "error")
        , identical(nl3[-1], nlis3[-1])
        , is.null(nl3[[1]])
)

## With different error messages
if(requireNamespace("nlme")) { # almost always, as it is recommended
 data(Soybean, package="nlme")
 attr(Soybean, "formula") #-> weight ~ Time | Plot  => split by "Plot":
 L <- lapply(split(Soybean, Soybean[,"Plot"]),
             function(DD) tryCatch(error = identity,
                 nls(weight ~ SSlogis(Time, Asym, xmid, scal), data = DD)))
 Lw <- warnErrList(L)
} # if <nlme>
}
\keyword{error}
\keyword{utilities}
