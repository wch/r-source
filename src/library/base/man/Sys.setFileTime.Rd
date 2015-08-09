% File src/library/base/man/Sys.info.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2011 R Core Team
% Distributed under GPL 2 or later

\name{Sys.setFileTime}
\alias{Sys.setFileTime}
\title{
  Set File Time
}
\description{
  Uses system calls to set the times on a file or directory.
}
\usage{
Sys.setFileTime(path, time)
}

\arguments{
  \item{path}{
    A length-one character vector specifying the path to a file or directory.
  }
  \item{time}{
    A date-time of class \code{"\link{POSIXct}"} or an object which can
    be coerced to one.  Fractions of a second are ignored.
  }
}
\details{
  This attempts sets the file time to the value specified.

  On a Unix-alike it uses the system call \code{utimes} if that is
  available, otherwise \code{utimes}.  On a POSIX file system it sets
  both the last-access and modification times.

  On Windows it uses the system call \code{SetFileTime} to set the
  \sQuote{last write time}.  Some Windows file systems only record the
  time at a resolution of two seconds.
}
\value{
  Logical, invisibly.  An indication if the operation succeeded.
}
\keyword{file}

