\name{tools-deprecated}
\alias{tools-deprecated}
\alias{filePathAsAbsolute}
\alias{filePathSansExt}
\alias{fileTest}
\alias{listFilesWithExts}
\alias{listFilesWithType}
\title{Deprecated Functions in Package tools}
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as of the next release.
}
\usage{
filePathAsAbsolute(x)
filePathSansExt(x)
fileTest(op, x, y)
listFilesWithExts(dir, exts, all.files = FALSE, full.names = TRUE)
listFilesWithType(dir, type, all.files = FALSE, full.names = TRUE)
}
\arguments{
  \item{x,y}{character vectors giving file paths.}
  \item{op}{a character string specifying the test to be performed.
    Unary tests (only \code{x} is used) are \code{"-f"} (existence and
    not being a directory) and \code{"-d"} (existence and directory);
    binary tests are \code{"-nt"} (newer than, using the modification
    dates) and \code{"-ot"}.}
  \item{dir}{a character string with the path name to a directory.}
  \item{exts}{a character vector of possible file extensions.}
  \item{all.files}{a logical.  If \code{FALSE} (default), only visible
    files are considered; if \code{TRUE}, all files are used.}
  \item{full.names}{a logical indicating whether the full paths of the
    files found are returned (default), or just the file names.}
  \item{type}{a character string giving the \dQuote{type} of the files
    to be listed, as characterized by their extensions.  Currently,
    possible values are \code{"code"} (R code), \code{"data"} (data
    sets), \code{"demo"} (demos), \code{"docs"} (R documentation), and
    \code{"vignette"} (vignettes).}
}
\seealso{
  \code{\link{Deprecated}}
}
\keyword{misc}
