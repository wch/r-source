% File src/library/base/man/file.access.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{file.access}
\alias{file.access}
\title{Ascertain File Accessibility}
\description{
  Utility function to access information about files on the user's
  file systems.
}
\usage{
file.access(names, mode = 0)
}
\arguments{
  \item{names}{character vector containing file names.
    Tilde-expansion will be done: see \code{\link{path.expand}}.}
  \item{mode}{integer specifying access mode required: see \sQuote{Details}.}
}
\details{
  The \code{mode} value can be the exclusive or of the following values
  \describe{
    \item{0}{test for existence.}
    \item{1}{test for execute permission.}
    \item{2}{test for write permission.}
    \item{4}{test for read permission.}
  }
#ifdef unix
  Permission will be computed for real user ID and real group ID (rather
  than the effective IDs).
#endif
#ifdef windows
  This function does uses the C function \code{_access} in
  \file{msvcrt.dll}, but was written using Win32 API functions.

  Windows does not have the concept of an \sQuote{executable file}, so
  this function regards directories and files with extension
  \file{.exe}, \file{.bat}, \file{.cmd} and \file{.com} as executable.
  (\code{\link{system}} and \code{\link{Sys.which}} make the same assumption.)

  UTF-8-encoded file names not valid in the current locale can be used.
#endif

  Please note that it is not a good idea to use this function to test
  before trying to open a file.  On a multi-tasking system, it is
  possible that the accessibility of a file will change between the time
  you call \code{file.access()} and the time you try to open the file.
  It is better to wrap file open attempts in \code{\link{try}}.
}
\value{
  An integer vector with values \code{0} for success and \code{-1} for failure.
}

\note{
  This is intended as a replacement for the S-PLUS function
  \code{access}, a wrapper for the C function of the same name, which
  explains the return value encoding.  Note that
  the return value is \bold{false} for \bold{success}.
}

\seealso{
  \code{\link{file.info}} for more details on permissions,
  \code{\link{Sys.chmod}} to change permissions, and
  \code{\link{try}} for a \sQuote{test it and see} approach.

  \code{\link{file_test}} for shell-style file tests.
}

\examples{
fa <- file.access(dir("."))
table(fa) # count successes & failures
#ifdef windows
d <- dir(file.path(R.home(), "bin"))
df <- dir(file.path(R.home(), "bin"), full.names = TRUE)
d[file.access(df, 0) == 0] # all exist
d[file.access(df, 1) == 0] # some are executable, some are not
d[file.access(df, 4) == 0] # hopefully all are readable
d[file.access(df, 2) == 0] # they may or may not be writable
#endif
}
\keyword{file}
