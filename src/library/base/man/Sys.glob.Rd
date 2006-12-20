\name{Sys.glob}
\alias{Sys.glob}
\title{Wildcard Expansion on File Paths}
\description{
  Function to do wildcard expansion (also known as \sQuote{globbing}) on
  file paths.

#ifdef unix
  Not all platforms support this.
#endif
}
\usage{
Sys.glob(paths, dirmark = FALSE)
}
\arguments{
  \item{paths}{character vector of patterns for relative or absolute
    filepaths.}
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
  \url{http://www.opengroup.org/onlinepubs/009695399/functions/glob.html})
  but some OSes will go beyond this (in particular some BSD-based OSes also
  do tilde expansion, see  \code{\link{path.expand}}).

  All systems should interpret \code{*} (match one of more characters),
  \code{?} (match a single character) and \code{[} (begin a character
  class or range).  If a filename starts with \code{.} this must be
  matched explicitly.  Normally paths ending in \code{/} will be
  accepted and matched only to directories.

  The rest of these details are indicative (and based on the POSIX
  standard).
  
  \code{[} begins a character class.  If the first character in
  \code{[...]} is not \code{!}, this is a character class which matches
  a single character against any of the characters specified.  The class
  cannot be empty, so \code{]} can be included provided it is first.  If
  the first character is \code{!}, the character class matches a single
  character which is none of the \emph{none} of the specified
  characters.
  
  Character classes can include ranges such as \code{[A-Z]}: include
  \code{-} as a character by having it first or last in a class.  (The
  interpretation of ranges should be locale-specific.)

  One can remove the special meaning of \code{?}, \code{*}  and
  \code{[} by preceding them by a backslash (except within a
  character class).
#endif
#ifdef windows
  The \code{glob} system call is not part of Windows, and we supply an
  emulation that works in bytes (and not in characters): it may not work
  correctly in CJK locales.

  Wildcards are \code{*} (match one of more characters), \code{?} (match
  a single character). If a filename starts with \code{.} this must be
  matched explicitly.

  In addition, \code{[} begins a character class.  If the first
  character in \code{[...]} is not \code{!}, this is a character class
  which matches a single character against any of the characters
  specified.  The class cannot be empty, so \code{]} can be included
  provided it is first.  If the first character is \code{!}, the
  character class matches a single character which is none of the
  \emph{none} of the specified characters.
  
  Character classes can include ranges such as \code{[A-Z]}: include
  \code{-} as a character by having it first or last in a class.  (In the
  current implementation ranges are in numeric order of bytes.)

  One can remove the special meaning of \code{?}, \code{*}  and
  \code{[} by preceding them by a backslash (except within a
  character class).

  File paths in Windows are interpreted with separator \code{\\} or
  \code{/}.  Paths with a drive but relative (such as \code{c:foo\\bar})
  are tricky, but an attempt is made to handle them correctly.
#endif
}
\value{
  A character vector of matched file paths.  The order is
  system-specific (but in the order of the elements of \code{paths}): it
  is normally collated in the current locale or in byte (ASCII) order.

  Directory errors are normally ignored, so the matches are to
  accessible file paths (but not necessarily accessible files).
#ifdef unix
  
  On platforms which do not have the \code{glob} system call,
  \code{paths} is returned unchanged.
#endif
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
