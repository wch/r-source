% File src/library/base/man/source.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{source}
\title{Read R Code from a File or a Connection}
\description{
  \code{source} causes \R to accept its input from the named file or URL
  or connection.  Input is read and \code{\link{parse}}d from that file
  until the end of the file is reached, then the parsed expressions are
  evaluated sequentially in the chosen environment.
}
\usage{
source(file, local = FALSE, echo = verbose, print.eval = echo,
       verbose = getOption("verbose"),
       prompt.echo = getOption("prompt"),
       max.deparse.length = 150, chdir = FALSE,
       encoding = getOption("encoding"),
       continue.echo = getOption("continue"),
       skip.echo = 0, keep.source = getOption("keep.source"))
}
\alias{source}
\arguments{
  \item{file}{a \link{connection} or a character string giving the pathname
    of the file or URL to read from.  \code{""} indicates the connection
    \code{\link{stdin}()}.}
  \item{local}{\code{TRUE}, \code{FALSE} or an environment, determining
    where the parsed expressions are evaluated.  \code{FALSE} (the
    default) corresponds to the user's workspace (the global
    environment) and \code{TRUE} to the environment from which
    \code{source} is called.}
  \item{echo}{logical; if \code{TRUE}, each expression is printed
    after parsing, before evaluation.}
  \item{print.eval}{logical; if \code{TRUE}, the result of
    \code{eval(i)} is printed for each expression \code{i}; defaults
    to the value of \code{echo}.}
  \item{verbose}{if \code{TRUE}, more diagnostics (than just
    \code{echo = TRUE}) are printed during parsing and evaluation of
    input, including extra info for \bold{each} expression.}
  \item{prompt.echo}{character; gives the prompt to be used if
    \code{echo = TRUE}.}
  \item{max.deparse.length}{integer; is used only if \code{echo} is
    \code{TRUE} and gives the maximal number of characters output for
    the deparse of a single expression.}
  \item{chdir}{logical; if \code{TRUE} and \code{file} is a pathname,
    the \R working directory is temporarily changed to the directory
    containing \code{file} for evaluating.}
  \item{encoding}{character vector.  The encoding(s) to be assumed when
    \code{file} is a character string: see \code{\link{file}}.  A
    possible value is \code{"unknown"} when the encoding is guessed: see
    the \sQuote{Encodings} section.}
  \item{continue.echo}{character; gives the prompt to use on
    continuation lines if \code{echo = TRUE}.}
  \item{skip.echo}{integer; how many comment lines at the start of the
    file to skip if \code{echo = TRUE}.}
  \item{keep.source}{logical: should the source formatting be retained
    when echoing expressions, if possible?}
}
\details{
  Note that running code via \code{source} differs in a few respects
  from entering it at the \R command line.  Since expressions are not
  executed at the top level, auto-printing is not done.  So you will
  need to include explicit \code{print} calls for things you want to be
  printed (and remember that this includes plotting by \CRANpkg{lattice},
  FAQ Q7.22).  Since the complete file is parsed before any of it is
  run, syntax errors result in none of the code being run.  If an error
  occurs in running a syntactically correct script, anything assigned
  into the workspace by code that has been run will be kept (just as
  from the command line), but diagnostic information such as
  \code{\link{traceback}()} will contain additional calls to
  \code{\link{withVisible}}.

  All versions of \R accept input from a connection with end of line
  marked by LF (as used on Unix), CRLF (as used on DOS/Windows) or CR
  (as used on classic Mac OS) and map this to newline.  The final line
  can be incomplete, that is missing the final end-of-line marker.

  If \code{keep.source} is true (the default in interactive use), the
  source of functions is kept so they can be listed exactly as input.

%   Using \code{echo = TRUE} and \code{keep.source = TRUE} may interact
%   badly with source code that includes \samp{#line nn "filename"}
%   directives (e.g., code produced by older versions of
%   \code{\link{Stangle}()}): \code{source()} will attempt to obtain the
%   source from the named file which may have changed since the code was
%   produced.  Use \code{keep.source = FALSE} to avoid this.

  Unlike input from a console, lines in the file or on a connection can
  contain an unlimited number of characters.

  When \code{skip.echo > 0}, that many comment lines at the start of
  the file will not be echoed.  This does not affect the execution of
  the code at all.  If there are executable lines within the first
  \code{skip.echo} lines, echoing will start with the first of them.

  If \code{echo} is true and a deparsed expression exceeds
  \code{max.deparse.length}, that many characters are output followed by
  \code{ .... [TRUNCATED] }.
}
\section{Encodings}{
  By default the input is read and parsed in  the current encoding of
  the \R session.  This is usually what it required, but occasionally
  re-encoding is needed, e.g.\sspace{}if a file from a UTF-8-using system is to
  be read on Windows (or \emph{vice versa}).

  The rest of this paragraph applies if \code{file} is an actual
  filename or URL (and not \code{""} nor a connection).  If
  \code{encoding = "unknown"}, an attempt is made to guess the encoding:
  the result of \code{\link{localeToCharset}()} is used as a guide.  If
  \code{encoding} has two or more elements, they are tried in turn until
  the file/URL can be read without error in the trial encoding.  If an
  actual \code{encoding} is specified (rather than the default or
  \code{"unknown"}) in a Latin-1 or UTF-8 locale then character strings
  in the result will be translated to the current encoding and marked as
  such (see \code{\link{Encoding}}).

  If \code{file} is a connection (including one specified by \code{""},
  it is not possible to re-encode the input inside \code{source}, and so
  the \code{encoding} argument is just used to mark character strings in the
  parsed input in Latin-1 and UTF-8 locales: see \code{\link{parse}}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{demo}} which uses \code{source};
  \code{\link{eval}}, \code{\link{parse}} and \code{\link{scan}};
  \code{\link{options}("keep.source")}.

  \code{\link{sys.source}} which is a streamlined version to source a
  file into an environment.

  \sQuote{The R Language Definition} for a discussion of source
  directives.
}
\examples{
## If you want to source() a bunch of files, something like
## the following may be useful:
 sourceDir <- function(path, trace = TRUE, ...) {
    for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
       if(trace) cat(nm,":")
       source(file.path(path, nm), ...)
       if(trace) cat("\n")
    }
 }
}
\keyword{file}
\keyword{programming}
\keyword{connection}
