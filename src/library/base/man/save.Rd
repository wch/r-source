\name{save}
\title{Save R Objects}
\usage{
save(\dots, list = character(0), file = "", ascii = FALSE,
     version = NULL, envir = parent.frame(), compress = FALSE)
save.image(file = ".RData", version = NULL, ascii = FALSE,
           compress = FALSE, safe = TRUE)

sys.load.image(name, quiet)
sys.save.image(name)
}
\alias{save}
\alias{save.image}
\alias{sys.save.image}
\alias{sys.load.image}
\arguments{
  \item{\dots}{the names of the objects to be saved.}
  \item{list}{A character vector containing the names of objects to be
    saved.}
  \item{file}{a connection or the name of the file where the data will be
    saved.  Must be a file name for workspace format version 1.}
  \item{ascii}{if \code{TRUE}, an ASCII representation of the data is
    written.  This is useful for transporting data between machines of
    different types.  The default value of \code{ascii} is \code{FALSE}
    which leads to a more compact binary file being written.}
  \item{version}{the workspace format version to use.  NULL specifies
    the current default format.  The version used from \R 0.99.0 to
    \R 1.3.1 was version 1.  The default format as from \R 1.4.0 is
    version 2.}
  \item{envir}{environment to search for objects to be saved.}
  \item{compress}{logical specifying whether saving to a named file is to
    use compression. Ignored when \code{file} is a connection and for
    workspace format version 1.}
  \item{safe}{logical.  If TRUE, a temporary file is used for creating
    the saved workspace.  The temporary file is renamed to \code{file}
    if the save succeeds.  This preserves an existing workspace
    \code{file} if the save fails, but at the cost of using extra disk
    space during the save.}
  \item{name}{name of image file to save or load.}
  \item{quiet}{logical specifying whether a message should be printed.}
  }
}
\description{
  \code{save} writes a external representation of \R objects to the
  specified file.  The objects can be read back from the file at a later
  date by using the function \code{load} (or \code{data} in some cases).

  \code{save.image()} is just a short-cut for ``save my current
  environment'',
  i.e., \code{save(list = ls(all=TRUE), file = ".RData")}. It is what also
  happens with \code{\link{q}("yes")}.
}
\details{
  All \R platforms use the XDR representation of binary objects in
  binary save-d files, and these are portable across all \R platforms.

  Default values for \code{save.image} options can be modified with
  the \code{save.image.defaults} option.  This mechanism is experimental
  and subject to change.

  \code{sys.save.image} is a system function that is called by \code{q()}
  and its GUI analogs; \code{sys.load.image} is called by the startup code.
  These functions should not be called directly and are subject to change.
}
\seealso{
  \code{\link{dput}}, \code{\link{dump}}, \code{\link{load}},
  \code{\link{data}}.
}
\examples{
x <- runif(20)
y <- list(a = 1, b = TRUE, c = "oops")
save(x, y, file = "xy.Rdata")
save.image()
unlink("xy.Rdata")
unlink(".RData")

# set save.image defaults using option:
options(save.image.defaults=list(ascii=TRUE, safe=FALSE))
save.image()
unlink(".RData")
}
\keyword{file}
