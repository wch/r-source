\name{message}
\alias{message}
\alias{suppressMessages}
\title{Diagnostic Messages}
\description{
  Generate a diagnostic message from its arguments.
}
\usage{
message(\dots, domain = NULL)
suppressMessages(expr)
}
\arguments{
  \item{\dots}{character vectors (which are pasted together with no
    separator), a condition object, or \code{NULL}.}
  \item{domain}{see \code{\link{gettext}}.
    If \code{NA}, messages will not be translated.}
  \item{expr}{expression to evaluate.}
}
\details{
  \code{message} is used for generating \dQuote{simple} diagnostic
  messages which are neither warnings nor errors, but nevertheless
  represented as conditions.

  While the message is being processed, a \code{muffleMessage} restart
  is available.
  
  \code{suppressMessages} evaluates its expression in a context that
  ignores all warnings.
}  
\seealso{
  \code{\link{warning}} and \code{\link{stop}} for generating warnings
  and errors; \code{\link{condition}} for condition handling and
  recovery.
  
  \code{\link{gettext}} for the mechanisms for the automated translation
  of messages.
}
\examples{
message("ABC", "DEF")
suppressMessages(message("ABC"))
}
\keyword{programming}
