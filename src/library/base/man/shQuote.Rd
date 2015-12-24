% File src/library/base/man/shQuote.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{shQuote}
\alias{shQuote}
\title{Quote Strings for Use in OS Shells}
\description{
  Quote a string to be passed to an operating system shell.
}
\usage{
shQuote(string, type = c("sh", "csh", "cmd", "cmd2"))
}
\arguments{
  \item{string}{a character vector, usually of length one.}
  \item{type}{character: the type of shell quoting.  Partial matching is
    supported.  \code{"cmd"} and \code{"cmd2"} refer to the Windows shell.
    \code{"cmd"} is the default under Windows.} 
}
\details{
  The default type of quoting supported under Unix-alikes is that for
  the Bourne shell \code{sh}.  If the string does not contain single
  quotes, we can just surround it with single quotes.  Otherwise, the
  string is surrounded in double quotes, which suppresses all special
  meanings of metacharacters except dollar, backquote and backslash, so
  these (and of course double quote) are preceded by backslash.  This
  type of quoting is also appropriate for \code{bash}, \code{ksh} and
  \code{zsh}.

  The other type of quoting is for the C-shell (\code{csh} and
  \code{tcsh}).  Once again, if the string does not contain single
  quotes, we can just surround it with single quotes.  If it does
  contain single quotes, we can use double quotes provided it does not
  contain dollar or backquote (and we need to escape backslash,
  exclamation mark and double quote).  As a last resort, we need to
  split the string into pieces not containing single quotes and surround
  each with single quotes, and the single quotes with double quotes.
  
  In Windows, command line interpretation is done by the application
  as well as the shell.  It may depend on the compiler used:
  Microsoft's rules for the C run-time are given at 
  \url{https://msdn.microsoft.com/library/ms880421}. It may depend on
  the whim of the programmer of the application: check its
  documentation.  The \code{type = "cmd"} quoting surrounds the string
  by double quotes and escapes internal double quotes by a backslash.
  As Windows path names cannot contain double quotes, this makes
  \code{shQuote} safe for use with many applications when used with
  \code{\link{system}} or \code{\link{system2}}.  The Windows
  \command{cmd.exe} shell (used by default with \code{\link{shell}})
  uses \code{type = "cmd2"} quoting:  special characters are prefixed
  with \code{"^"}.  In some cases, two types of quoting should be
  used:  first for the application, and then \code{type = "cmd2"} 
  for \command{cmd.exe}.  See the examples below.  
}
  
\references{ Loukides, M. \emph{et al} (2002) \emph{Unix Power Tools}
  Third Edition. O'Reilly.  Section 27.12.
  
  Discussion in \PR{16636}.

% gone in Jan 2015  
% \url{http://www.mhuffman.com/Notes/dos/bash_cmd.htm}
}
\seealso{
  \link{Quotes} for quoting \R code.

  \code{\link{sQuote}} for quoting English text.
}
\examples{
test <- "abc$def`gh`i\\\\j"
cat(shQuote(test), "\n")
\dontrun{system(paste("echo", shQuote(test)))}
test <- "don't do it!"
cat(shQuote(test), "\n")

tryit <- paste("use the", sQuote("-c"), "switch\nlike this")
cat(shQuote(tryit), "\n")
\dontrun{system(paste("echo", shQuote(tryit)))}
cat(shQuote(tryit, type = "csh"), "\n")

## Windows-only example, assuming cmd.exe:
perlcmd <- 'print "Hello World\\\\n";'
\dontrun{
shell(shQuote(paste("perl -e", 
                    shQuote(perlcmd, type = "cmd")),
              type = "cmd2"))
}
}
\keyword{utilities}
