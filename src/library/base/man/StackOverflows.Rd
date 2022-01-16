% File src/library/base/man/StackOverflows.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2021 R Core Team
% Distributed under GPL 2 or later

\name{StackOverflows}
\alias{StackOverflows}
\alias{stackOverflowError}
\alias{CStackOverflowError}
\alias{expressionStackOverflowError}
\alias{protectStackOverflowError}
\alias{nodeStackOverflowError}
\title{Stack Overflow Errors}
\description{
  Errors signaled by \R when stacks used in evaluation overflow.
}
\details{

  \R uses several stacks in evaluating expressions: the C stack, the
  pointer protection stack, and the node stack used by the byte code
  engine. In addition, the number of nested \R expressions currently
  under evaluation is limited by the value set as
  \code{\link{options}("expressions")}.  Overflowing these stacks or
  limits signals an error that inherits from classes
  \code{stackOverflowError}, \code{error}, and \code{condition}.

  The specific classes signaled are:

  \itemize{
    \item \code{CStackOverflowError}: Signaled when the C stack
    overflows. The \code{usage} field of the error object contains the
    current stack usage.

    \item \code{protectStackOverflowError}: Signaled when the pointer
    protection stack overflows.

    \item \code{nodeStackOverflowError}: Signaled when the node stack
    used by the byte code engine overflows.

    \item \code{expressionStackOverflowError}: Signaled when the the
    evaluation depth, the number of nested \R expressions currently
    under evaluation, exceeds the limit set by
    \code{\link{options}("expressions")}
  }

  Stack overflow errors can be caught and handled by exiting handlers
  established with \code{\link{tryCatch}()} Calling handlers established
  by \code{\link{withCallingHandlers}()} may fail since there may not be
  enough stack space to run the handler. In this case the next available
  exiting handler will be run, or error handling will fall back to the
  default handler. Default handlers set by
  \code{\link{tryCatch}("error")} may also fail to run in a stack
  overflow situation.
}

\seealso{
  \code{\link{Cstack_info}} for information on the environment and the
  evaluation depth limit.

  \code{\link{Memory}} and \code{\link{options}} for information on the
  protection stack.
}

\keyword{programming}
\keyword{error}
\keyword{classes}
