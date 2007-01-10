\name{message}
\alias{message}
\alias{suppressMessages}
\alias{packageStartupMessage}
\alias{.packageStartupMessage}
\alias{suppressPackageStartupMessages}
\title{Diagnostic Messages}
\description{
  Generate a diagnostic message from its arguments.
}
\usage{
message(\dots, domain = NULL, appendLF = TRUE, type = simpleMessage)
suppressMessages(expr)

packageStartupMessage(\dots, domain = NULL, appendLF = TRUE)
suppressPackageStartupMessages(expr)
}
\arguments{
  \item{\dots}{objects with can be coerced to character vectors (which
    are pasted together with no separator) or a condition object.}
  \item{domain}{see \code{\link{gettext}}.
    If \code{NA}, messages will not be translated.}
  \item{appendLF}{logical: should messages given as a character string
    have a newline appended?}
  \item{type}{generator for the condtion: should have the same interface
    as \code{\link{simpleMessage}}.}
  \item{expr}{expression to evaluate.}
}
\details{
  \code{message} is used for generating \dQuote{simple} diagnostic
  messages which are neither warnings nor errors, but nevertheless
  represented as conditions.  Unlile warnings and errors, a final
  newline is regarded as part of the message, and is optional.

  While the message is being processed, a \code{muffleMessage} restart
  is available.
  
  \code{suppressMessages} evaluates its expression in a context that
  ignores all \dQuote{simple} diagnostic messages.

  \code{packageStartupMessage} is a variant whose messages can be
  suppressed separately by \code{suppressPackageStartupMessages}.  (They
  are still messages, so can be suppressed by \code{suppressMessages}.)

  Argument \code{type} can be used to set up classes of messages with
  separate properties: this is how \code{packageStartupMessage} works,
  using the condition generator \code{.packageStartupMessage} which
  prepends a class to \code{simpleMessage}.
}  
\seealso{
  \code{\link{warning}} and \code{\link{stop}} for generating warnings
  and errors; \code{\link{conditions}} for condition handling and
  recovery.
  
  \code{\link{gettext}} for the mechanisms for the automated translation
  of text.
}
\examples{
message("ABC", "DEF")
suppressMessages(message("ABC"))

testit <- function() {
  message("testing package startup messages")
  packageStartupMessage("initializing ...", appendLF = FALSE)
  Sys.sleep(1)
  packageStartupMessage(" done")
}

testit()
suppressPackageStartupMessages(testit())
suppressMessages(testit())
}
\keyword{programming}
