\name{save}
\alias{save}
\alias{save.image}
\title{Save R Objects}
\description{
  \code{save} writes an external representation of \R objects to the
  specified file.  The objects can be read back from the file at a later
  date by using the function \code{load} (or \code{data} in some cases).

  \code{save.image()} is just a short-cut for \dQuote{save my current
    workspace},
  i.e., \code{save(list = ls(all=TRUE), file = ".RData")}.  It is also what
  happens with \code{\link{q}("yes")}.
}
\usage{
save(\dots, list = character(0),
     file = stop("'file' must be specified"),
     ascii = FALSE, version = NULL, envir = parent.frame(),
     compress = !ascii, eval.promises = TRUE)

save.image(file = ".RData", version = NULL, ascii = FALSE,
           compress = !ascii, safe = TRUE)
}
\arguments{
  \item{\dots}{the names of the objects to be saved.}
  \item{list}{A character vector containing the names of objects to be
    saved.}
  \item{file}{a connection or the name of the file where the data will be
    saved.  Must be a file name for workspace format version 1.}
  \item{ascii}{if \code{TRUE}, an ASCII representation of the data is
    written.  The default value of \code{ascii} is \code{FALSE} which
    leads to a more compact binary file being written.}
  \item{version}{the workspace format version to use.  \code{NULL}
    specifies the current default format.  The version used from \R
    0.99.0 to \R 1.3.1 was version 1.  The default format as from \R
    1.4.0 is version 2.}
  \item{envir}{environment to search for objects to be saved.}
  \item{compress}{logical specifying whether saving to a named file is to
    use compression.  Ignored when \code{file} is a connection and for
    workspace format version 1.}
  \item{eval.promises}{logical: should objects which are promises be
    forced before saving?}
  \item{safe}{logical.  If \code{TRUE}, a temporary file is used for
    creating the saved workspace.  The temporary file is renamed to
    \code{file} if the save succeeds.  This preserves an existing
    workspace \code{file} if the save fails, but at the cost of using
    extra disk space during the save.}
}
\details{
  The names of the objects specified either as symbols in \code{\dots} or
  as a character vector in \code{list} are used to look up the objects
  from environment \code{envir}.  By default \link{promises} are
  evaluated, but if \code{eval.promises = FALSE} promises are saved
  (together with their evaluation environments).  (Promises embedded in
  objects are always saved unevaluated.)

  All \R platforms use the XDR representation of binary objects in
  binary save-d files, and these are portable across all \R platforms.
  (ASCII saves used to be useful for moving data between platforms but
  are now only of historical interest.)

  Default values for the \code{ascii}, \code{compress}, \code{safe} and
  \code{version} arguments can be modified with the \code{save.defaults}
  option (used both by \code{save} and \code{save.image}), see also the
  example section below.  If a \code{save.image.defaults} option is set
  it overrides \code{save.defaults} for function \code{save.image}
  (which allows this to have different defaults).

  It is possible to compress later (with \code{gzip}) a file saved with
  \code{compress = FALSE}: the effect is the same as saving with
  \code{compress = TRUE}.
}

\note{
  The defaults were changed to use compressed saves for \code{save} in
  2.3.0 and for \code{save.image} in 2.4.0.  Any recent version of \R
  can read compressed save files, and a compressed file can be
  uncompressed (by \code{gzip -d}) for use with very old versions of \R.
}

\section{Warnings}{
  The \code{\dots} arguments only give the \emph{names} of the objects
  to be saved: they are searched for in the environment given by the
  \code{envir} argument, and the actual objects given as arguments need
  not be those found.

  Saved \R objects are binary files, even those saved with
  \code{ascii = TRUE}, so ensure that they are transferred without
  conversion of end of line markers and of 8-bit characters.  The lines
  are delimited by LF on all platforms.

  Although the default version has not changed since \R 1.4.0, this
  does not mean that saved files are necessarily backwards compatible.
  You will be able to load a saved image into an earlier version of \R
  unless use is made of later additions (for example, raw vectors or
  external pointers).
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

# set save defaults using option:
options(save.defaults=list(ascii=TRUE, safe=FALSE))
save.image()
unlink(".RData")
}
\keyword{file}
