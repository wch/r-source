% File src/library/utils/man/filetest.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{file_test}
\alias{file_test}
\title{
  Shell-style Tests on Files
}
\description{
  Utility for shell-style file tests.
}
\usage{
file_test(op, x, y)
}
\arguments{
  \item{op}{a character string specifying the test to be performed.
    Unary tests (only \code{x} is used) are \code{"-f"} (existence and
    not being a directory), \code{"-d"} (existence and directory) and
    \code{"-x"} (executable as a file or searchable as a directory).
    Binary tests are \code{"-nt"} (strictly newer than, using the modification
    dates) and \code{"-ot"} (strictly older than): in both cases the
    test is false unless both files exist.}
  \item{x, y}{character vectors giving file paths.}
}
\details{
  \sQuote{Existence} here means being on the file system and accessible
  by the \code{stat} system call (or a 64-bit extension) -- on a
  Unix-alike this requires execute permission on all of the directories in
  the path that leads to the file, but no permissions on the file
  itself.

  For the meaning of \code{"-x"} on Windows see \code{\link{file.access}}.
}
\seealso{
  \code{\link{file.exists}} which only tests for existence
  (\code{test -e} on some systems) but not for not being a directory.

  \code{\link{file.path}}, \code{\link{file.info}}
}
\examples{
dir <- file.path(R.home(), "library", "stats")
file_test("-d", dir)
file_test("-nt", file.path(dir, "R"), file.path(dir, "demo"))
}
\keyword{file}
