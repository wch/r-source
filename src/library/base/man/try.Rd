\name{try}
\alias{try}
\title{Try an Expression Allowing Error Recovery.}
\usage{
try(expr, first = TRUE)
}
\arguments{
  \item{expr}{an \R expression to try}
  \item{first}{not for user use!}
}
\description{
  \code{try} is a wrapper to run an expression that might fail and allow
  the user's code to handle error-recovery.
}
\details{
  \code{try} is a user-friendly wrapper to \code{\link{restart}}.
  The argument \code{first} is used to record if \code{restart} has
  already been used, and so ensure that \code{restart} is called only
  once.
}
\value{
  The value of the expression if \code{expr} is evaluated without error,
  but an invisible object of class \code{"try-error"} containing the
  error message if it if fails. The normal error handling will print the
  same message unless \code{options("show.error.messages")} is false.
}
\seealso{
  \code{\link{options}} for setting error handlers and suppressing the
  printing of error messages;
  \code{\link{geterrmessage}} for retrieving the last error message.
}
\examples{
## this example will not work correctly in example(try), but
## it does work correctly if pasted in
options(show.error.messages = FALSE)
try(log("a"))
print(.Last.value)
options(show.error.messages = TRUE)

## run a simulation, keep only the results that worked.
set.seed(123)
x <- rnorm(50)
doit <- function(x)
{
    x <- sample(x, replace=TRUE)
    if(length(unique(x)) > 30) mean(x)
    else stop("too few unique points")
}
options(show.error.messages = FALSE)
## alternative 1
res <- lapply(1:100, function(i) try(doit(x)))
## alternative 2
\dontrun{res <- vector("list", 100)
for(i in 1:100) res[[i]] <- try(doit(x))}
options(show.error.messages = TRUE)
unlist(res[sapply(res, function(x) !inherits(x, "try-error"))])
}
\keyword{programming}
