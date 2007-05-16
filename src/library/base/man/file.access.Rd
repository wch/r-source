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
  \item{names}{character vector containing file names.}
  \item{mode}{integer specifying access mode required.}
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
  As from \R 2.6.0 this function no longer uses the C function
  \code{_access} in \file{msvcrt.dll}, but has been rewritten using
  Win32 API functions.

  Windows does not have the concept of an \sQuote{executable file}, so
  this function regards directories and files with extension
  \file{.exe}, \file{.bat}, \file{.cmd} and \file{.com} as executable.
  (\code{\link{system}} makes the same assumption.)
#endif

  Please note that it is not good to use this function to test before
  trying to open a file.  On a multi-tasking system, it is possible that
  the accessibility of a file will change between the time you call
  \code{file.access()} and the time you try to open the file.  It is
  better to wrap file open attempts in \code{\link{try}}.
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

\seealso{\code{\link{file.info}}, \code{\link{try}}}

\examples{
fa <- file.access(dir("."))
table(fa) # count successes & failures
#ifdef windows
d <- dir(file.path(R.home(), "bin"))
df <- dir(file.path(R.home(), "bin"), full.names = TRUE)
d[file.access(df, 0) == 0] # all exist
d[file.access(df, 1) == 0] # some are executable, some are not
d[file.access(df, 2) == 0] # hopefully all are readable
d[file.access(df, 4) == 0] # they may or may not be writeable
#endif
}
\keyword{file}
