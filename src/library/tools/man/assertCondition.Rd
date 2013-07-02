\name{assertCondition}
\alias{assertCondition}
\title{Asserting Error Conditions}
\description{
  When testing code, it is not sufficient to check that results are correct,
  but also that errors or warnings are signalled in appropriate
  situations.  \code{assertCondition()} allows to perform such checks
  conveniently.
}
\usage{
assertCondition(expr, ..., verbose=getOption("verbose"),
	        ignoreWarning = is.na(match("warning", conds)))
}
\arguments{
  \item{expr}{an unevaluated \R expression which will be evaluated via
    \code{\link{tryCatch}(expr, ..)}.}
  \item{\dots}{\code{\link{character}} strings such as \code{"error"} or
    \code{"warning"}; defaults to \code{"condition"}.}
  \item{verbose}{logical indicating if the message should be shown even
    when the condition is asserted.}
  \item{ignoreWarning}{logical indicating if \code{"warning"} messages
    should be caught.  In that case, the warning is typically caught
    \emph{before} an error can be signalled, and so an \code{"error"} can
    not be asserted.}
}
%% \details{
%% }
\value{
  if the \code{assertCondition(*)} does not signal an error, this means that
  \code{expr} \emph{did} signal a condition, and that  returned
  invisibly in this case.
}
\author{John Chambers (and Martin Maechler)}
\seealso{
  \code{\link{stop}}, \code{\link{warning}};
  \code{\link{signalCondition}}, \code{\link{tryCatch}}, etc.
}
\examples{
assertCondition( ""-1 ) # ok, as it signals a "condition"

try( assertCondition(sqrt(2), "warning") )
## .. Failed to get expected warning in evaluating sqrt(2)
     assertCondition(sqrt("abc"), "error")   # ok
try( assertCondition(sqrt("abc"), "warning") )# -> error: had no warning
## verbose ==> show error message additionally:
     assertCondition(sqrt("abc"), verbose=TRUE, "error")

assertCondition(matrix(1:5, 2,3), "warning")
try( assertCondition(matrix(1:8, 4,3), "error") )
## .. Failed to get expected error ....

## either warning or worse:
assertCondition(matrix(1:8, 4,3), verbose=TRUE, "error","warning") # OK
assertCondition(matrix(1:8, 4, 3), "warning", "error") # ditto

## when both are signalled:
ff <- function() { warning("my warning"); stop("my error") }
    assertCondition(ff(), "warning")
    assertCondition(ff(), "error")          # ok + signalling the warning
    assertCondition(ff(), "error", ignoreWarning=TRUE)   # ditto
try(assertCondition(ff(), "error", ignoreWarning=FALSE) )# not ok:
                                          #-> gets warning instead of error
assertCondition(ff(), "error", "warning") # ok (quietly, catching warning)
}
\keyword{programming}
\keyword{error}
