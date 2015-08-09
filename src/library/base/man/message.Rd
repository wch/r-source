% File src/library/base/man/message.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{message}
\alias{message}
\alias{suppressMessages}
\alias{packageStartupMessage}
\alias{.packageStartupMessage}
\alias{suppressPackageStartupMessages}
\alias{.makeMessage}
\title{Diagnostic Messages}
\description{
  Generate a diagnostic message from its arguments.
}
\usage{
message(\dots, domain = NULL, appendLF = TRUE)
suppressMessages(expr)

packageStartupMessage(\dots, domain = NULL, appendLF = TRUE)
suppressPackageStartupMessages(expr)

.makeMessage(\dots, domain = NULL, appendLF = FALSE)
}
\arguments{
  \item{\dots}{zero or more objects which can be coerced to character
    (and which are pasted together with no separator) or (for
    \code{message} only) a single condition object.}
  \item{domain}{see \code{\link{gettext}}.  If \code{NA}, messages will
    not be translated, see also the note in \code{\link{stop}}.}
  \item{appendLF}{logical: should messages given as a character string
    have a newline appended?}
  \item{expr}{expression to evaluate.}
}
\details{
  \code{message} is used for generating \sQuote{simple} diagnostic
  messages which are neither warnings nor errors, but nevertheless
  represented as conditions.  Unlike warnings and errors, a final
  newline is regarded as part of the message, and is optional.
  The default handler sends the message to the
  \code{\link{stderr}()} \link{connection}.

  If a condition object is supplied  to \code{message} it should be
  the only argument, and further arguments will be ignored, with a warning.

  While the message is being processed, a \code{muffleMessage} restart
  is available.

  \code{suppressMessages} evaluates its expression in a context that
  ignores all \sQuote{simple} diagnostic messages.

  \code{packageStartupMessage} is a variant whose messages can be
  suppressed separately by \code{suppressPackageStartupMessages}.  (They
  are still messages, so can be suppressed by \code{suppressMessages}.)

  \code{.makeMessage} is a utility used by \code{message}, \code{warning}
  and \code{stop} to generate a text message from the \code{\dots}
  arguments by possible translation (see \code{\link{gettext}}) and
  concatenation (with no separator).
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
