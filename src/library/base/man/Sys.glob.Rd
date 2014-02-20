% File src/library/base/man/Sys.glob.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{Sys.glob}
\alias{Sys.glob}
\title{Wildcard Expansion on File Paths}
\description{
  Function to do wildcard expansion (also known as \sQuote{globbing}) on
  file paths.
}
\usage{
Sys.glob(paths, dirmark = FALSE)
}
\arguments{
  \item{paths}{character vector of patterns for relative or absolute
    filepaths.  Missing values will be ignored.}
  \item{dirmark}{logical: should matches to directories from patterns
    that do not already end in \code{/}
#ifdef windows
    or \code{\\}
#endif
    have a slash appended?  May not be supported on all platforms.}
}
\details{
#ifdef unix
  This expands wildcards in file paths.  For precise details, see your
  system's documentation on the \code{glob} system call.  There is a
  POSIX 1003.2 standard (see
  \url{http://pubs.opengroup.org/onlinepubs/9699919799/functions/glob.html})
  but some OSes will go beyond this.  The \R implementation will always
  do \link{tilde expansion}.

  All systems should interpret \code{*} (match zero or more characters),
  \code{?} (match a single character) and (probably) \code{[} (begin a
  character class or range).  The handling of paths
  ending with a separator is system-dependent.  On a POSIX-2008
  compliant OS they will match directories (only), but as they are not
  valid filepaths on Windows, they match nothing there.  (Earlier POSIX
  standards allowed them to match files.)
  % section A.4.12, http://pubs.opengroup.org/onlinepubs/9699919799/xrat/V4_xbd_chap04.html

  The rest of these details are indicative (and based on the POSIX
  standard).

  If a filename starts with \code{.} this may need to be matched
  explicitly: for example \code{Sys.glob("*.RData")} may or may not
  match \file{.RData} but will not usually match \file{.aa.RData}.  Note
  that this is platform-dependent: e.g. on Solaris \code{Sys.glob("*.*")}
  matches \file{.} and \file{..}.

  \code{[} begins a character class.  If the first character in
  \code{[...]} is not \code{!}, this is a character class which matches
  a single character against any of the characters specified.  The class
  cannot be empty, so \code{]} can be included provided it is first.  If
  the first character is \code{!}, the character class matches a single
  character which is \emph{none} of the specified characters.  Whether
  \code{.} in a character class matches a leading \code{.} in the
  filename is OS-dependent.

  Character classes can include ranges such as \code{[A-Z]}: include
  \code{-} as a character by having it first or last in a class.  (The
  interpretation of ranges should be locale-specific, so the example is
  not a good idea in an Estonian locale.)

  One can remove the special meaning of \code{?}, \code{*}  and
  \code{[} by preceding them by a backslash (except within a
  character class).
#endif
#ifdef windows
  The \code{glob} system call is not part of Windows, and we supply a
  partial emulation.

  Wildcards are \code{*} (match zero or more characters) and \code{?} (match
  a single character).  If a filename starts with \code{.} this must be
  matched explicitly (on Windows, but note that this is platform-dependent).

  \code{[} begins a character class.  If the first character in
  \code{[...]} is not \code{!}, this is a character class which matches
  a single character against any of the characters specified.  The class
  cannot be empty, so \code{]} can be included provided it is first.  If
  the first character is \code{!}, the character class matches a single
  character which is \emph{none} of the specified characters.  Whether
  \code{.} in a character class matches a leading \code{.} in the
  filename is OS-dependent.

  Character classes can include ranges such as \code{[A-Z]}: include
  \code{-} as a character by having it first or last in a class.  (In
  the current implementation ranges are in the numeric order of Unicode
  code points.)

  One can remove the special meaning of \code{?}, \code{*}  and
  \code{[} by preceding them by a backslash (except within a
  character class).  Note that on Windows \code{?} and \code{*} are not
  valid in file names, so this is mainly for consistency with other
  platforms.

  File paths in Windows are interpreted with separator \code{\\} or
  \code{/}.  Paths with a drive but relative (such as \code{c:foo\\bar})
  are tricky, but an attempt is made to handle them correctly.  An
  attempt is made to handle UNC paths starting with a double
  backslash. UTF-8-encoded paths not valid in the current locale can be
  used.
#endif
}
\value{
  A character vector of matched file paths.  The order is
  system-specific (but in the order of the elements of \code{paths}): it
  is normally collated in either the current locale or in byte (ASCII)
  order; however, on Windows collation is in the order of Unicode
  points.

  Directory errors are normally ignored, so the matches are to
  accessible file paths (but not necessarily accessible files).
}
\seealso{
  \code{\link{path.expand}}.

  \link{Quotes} for handling backslashes in character strings.
}
\examples{
\dontrun{
Sys.glob(file.path(R.home(), "library", "*", "R", "*.rdx"))
}}
\keyword{utilities}
\keyword{file}
