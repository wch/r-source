\name{assertCondition}
\alias{assertCondition}
\alias{assertWarning}
\alias{assertError}
\title{Asserting Error Conditions}
\description{
  When testing code, it is not sufficient to check that results are correct,
  but also that errors or warnings are signalled in appropriate
  situations.  The functions described here provide a convenient
  facility for doing so.  The three functions check that evaluating the
  supplied expression produces an error, a warning or one of a
  specified list of conditions, respectively.  If the assertion fails,
  an error is signalled.
}
\usage{
assertError(expr, classes = "error", verbose = FALSE)
assertWarning(expr, classes = "warning", verbose = FALSE)
assertCondition(expr, ..., .exprString = , verbose = FALSE)
}
\arguments{
  \item{expr}{an unevaluated \R expression which will be evaluated via
    \code{\link{tryCatch}(expr, ..)}.}
  \item{\dots}{\code{\link{character}} strings corresponding to the
    classes of the conditions that would satisfy the assertion; e.g., \code{"error"} or
    \code{"warning"}.  If none are specified, any condition will
    satisfy the assertion.  See the details section. }
  \item{.exprString}{The string to be printed corresponding to
    \code{expr}. By default, the actual \code{expr} will be
    deparsed.  Will be omitted if the function is supplied with the
    actual expression to be tested.  If \code{assertCondition()} is
    called from another function, with the actual expression passed as
    an argument to that function, supply the deparsed version.}
  \item{verbose}{If \code{TRUE}, a message is printed when the
    condition is satisfied.}
}
\details{
  \code{assertCondition()} uses the general condition mechanism to
  check all the conditions generated in evaluating \code{expr}.  The
  occurrence of any of the supplied condition classes among these satisfies the
  assertion regardless of what other conditions may be signalled.

  \code{assertError()} is a convenience function for asserting errors;
  it calls \code{assertCondition()}.

  \code{assertWarning()} asserts that a warning will be signalled, but
  \emph{not} an error, whereas \code{assertCondition(expr, "warning")}
  will be satisfied even if an error follows the warning.  See the examples.
 }
\value{
  If the assertion is satisfied, a list of all the condition objects
  signalled is returned, invisibly. See \code{\link{conditionMessage}} for the
  interpretation of these objects.  Note that \emph{all} conditions
  signalled during the evaluation are returned, whether or not they
  were among the requirements.
}
\author{John Chambers and Martin Maechler}
\seealso{
  \code{\link{stop}}, \code{\link{warning}};
  \code{\link{signalCondition}}, \code{\link{tryCatch}}.
}
\examples{
  assertError(sqrt("abc"))
  assertWarning(matrix(1:8, 4,3))

  assertCondition( ""-1 ) # ok, any condition would satisfy this

try( assertCondition(sqrt(2), "warning") )
## .. Failed to get warning in evaluating sqrt(2)
     assertCondition(sqrt("abc"), "error")   # ok
try( assertCondition(sqrt("abc"), "warning") )# -> error: had no warning
     assertCondition(sqrt("abc"), "error")
  ## identical to assertError() call above

assertCondition(matrix(1:5, 2,3), "warning")
try( assertCondition(matrix(1:8, 4,3), "error") )
## .. Failed to get expected error ....

## either warning or worse:
assertCondition(matrix(1:8, 4,3), "error","warning") # OK
assertCondition(matrix(1:8, 4, 3), "warning") # OK

## when both are signalled:
ff <- function() { warning("my warning"); stop("my error") }
    assertCondition(ff(), "warning")
## but assertWarning does not allow an error to follow
try(assertWarning(ff()))
    assertCondition(ff(), "error")          # ok
assertCondition(ff(), "error", "warning") # ok (quietly, catching warning)

## assert that assertC..() does not assert [and use *one* argument only]
assertCondition( assertCondition(sqrt( 2   ), "warning") )
assertCondition( assertCondition(sqrt("abc"), "warning"), "error")
assertCondition( assertCondition(matrix(1:8, 4,3), "error"),
                "error")
}
\keyword{programming}
\keyword{error}
