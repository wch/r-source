\name{shQuote}
\alias{shQuote}
\title{Quote Strings for Use in OS Shells}
\description{
  Quote a string to be passed to an operating system shell.
}
\usage{
shQuote(string, type = c("sh", "csh", "cmd"))
}
\arguments{
  \item{string}{a character vector, usually of length one.}
  \item{type}{character: the type of shell.  Partial matching is
    supported.  \code{"cmd"} refers to the Windows NT shell, and is the
    default under Windows.}
}
\details{
  The default type of quoting supported under Unix-alikes is that for
  the Bourne shell \code{sh}.  If the string does not contain single
  quotes, we can just surround it with single quotes.  Otherwise, the
  string is surrounded in double quotes, which suppresses all special
  meanings of metacharacters except dollar, backquote and backslash, so
  these (and of course double quote) are preceded by backslash.  This
  type of quoting is also appropriate for \code{ksh}, \code{zsh} and
  \code{bash}.

  The other type of quoting is for the C-shell (\code{csh} and
  \code{tcsh}).  Once again, if the string does not contain single
  quotes, we can just surround it with single quotes.  If it does
  contain single quotes, we can use double quotes provided it does not
  contain dollar or backquote (and we need to escape backslash,
  exclamation mark and double quote).  As a last resort, we need to
  split the string into pieces not containing single quotes and surround
  each with single quotes, and the single quotes with double quotes.

#ifdef windows
  The Windows shell supports only double quoting.  All this
  implementation does is to surround by double quotes and escape
  internal double quotes by a backslash.  As Windows path names cannot
  contain double quotes, this makes \code{shQuote} safe for use with
  \code{\link{system}} and with \code{\link{shell}} if the default shell
  is used.

  It will usually be safe to use \code{shQuote} to quote arguments of a
  command, but because \code{\link{system}} does not use a shell,
  interpretation of quoted arguments is done by the run-time code of the
  executable.
#endif
}
\references{
  Loukides, M. et al (2002)
  \emph{Unix Power Tools} Third Edition.  O'Reilly.  Section 27.12.

  \url{http://www.mhuffman.com/notes/dos/bash_cmd.htm}
}
\seealso{
  \code{Quotes} for quoting \R code.

  \code{\link{sQuote}} for quoting English text.
}
\examples{
test <- "abc$def`gh`i\\j"
cat(shQuote(test), "\n")
\dontrun{system(paste("echo", shQuote(test)))}
test <- "don't do it!"
cat(shQuote(test), "\n")

tryit <- "use the `-c' switch\nlike this"
cat(shQuote(tryit), "\n")
\dontrun{system(paste("echo", shQuote(tryit)))}
cat(shQuote(tryit, type="csh"), "\n")

## Windows-only example.
perlcmd <- 'print "Hello World\\n";'
\dontrun{shell(paste("perl -e", shQuote(perlcmd, type="cmd")))}
}
\keyword{utilities}
