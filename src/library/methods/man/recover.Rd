\name{recover}
\alias{recover}
\title{Browsing on an Error }
\description{
  This function allows the user to browse directly on any of the
  currently active function calls, and is suitable as an error option.
  The expression \code{options(error=recover)} will make this
  the error option.
}
\usage{
recover()
}
\details{
  When called, \code{recover} prints the list of current calls, and
  prompts the user to select one of them.  The standard R
  \code{\link{browser}} is then invoked from the corresponding
  environment; the user can type ordinary S language expressions to be
  evaluated in that environment.

  When finished browsing in this call, type \code{c} to return to
  \code{recover} from the browser.  Type another frame number to browse
  some more, or type \code{0} to exit \code{recover}.

  \emph{WARNING}: Do not use the special \code{Q} command to go directly
  from the browser to the prompt level of the evaluator.  This currently
  interacts with \code{recover} to effectively turn off the error option
  for the next error (on subsequent errors, \code{recover} will be called normally)
  }
\value{
 Nothing useful is returned.  However, you \emph{can} invoke
 \code{recover} directly from a function, rather than through the error option shown
 the usage section.  In this case, execution continues after you type
 \code{0} to exit \code{recover}.
}

\section{Compatibility Note}{
  The R \code{recover} function can be used in the same way as the
  S-Plus function of the same name; therefore, the error option shown is
  a compatible way to specify the error action.  However, the actual
  functions are essentially unrelated and interact quite differently
  with the user.
}

\references{\emph{Programming with Data}, (John M. Chambers, Springer,
  1998).  See the compatibility note above, however.}

\seealso{ See \code{\link{dump.frames}} and \code{\link{debugger}} for
  the more traditional approach to error recovery.}

\keyword{programming}
