\name{file_test}
\alias{file_test}
\title{
  Shell-style Tests on Files
}
\description{
  Utility for testing files.
}
\usage{
file_test(op, x, y)
}
\arguments{
  \item{op}{a character string specifying the test to be performed.
    Unary tests (only \code{x} is used) are \code{"-f"} (existence and
    not being a directory) and \code{"-d"} (existence and directory);
    binary tests are \code{"-nt"} (newer than, using the modification
    dates) and \code{"-ot"}.}
  \item{x,y}{character vectors giving file paths.}
}
\details{
  \code{file_test} performs shell-style file tests.

  Note that \code{\link{file.exists}} only tests for existence
  (\code{test -e} on some systems) but not for not being a directory.
}
\seealso{
  \code{\link{file.path}},
  \code{\link{file.info}}
}
\examples{
dir <- file.path(R.home(), "library", "stats")
file_test("-d", dir)
file_test("-nt", file.path(dir, "R"), file.path(dir, "demo"))
}
\keyword{file}
