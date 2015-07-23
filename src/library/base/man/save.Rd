% File src/library/base/man/save.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{save}
\alias{save}
\alias{save.image}
\title{Save R Objects}
\description{
  \code{save} writes an external representation of \R objects to the
  specified file.  The objects can be read back from the file at a later
  date by using the function \code{\link{load}} or \code{\link{attach}}
  (or \code{\link{data}} in some cases).

  \code{save.image()} is just a short-cut for \sQuote{save my current
    workspace}, i.e., \code{save(list = ls(all = TRUE), file =
    ".RData")}.  It is also what happens with \code{\link{q}("yes")}.
}
\usage{
save(\dots, list = character(),
     file = stop("'file' must be specified"),
     ascii = FALSE, version = NULL, envir = parent.frame(),
     compress = isTRUE(!ascii), compression_level,
     eval.promises = TRUE, precheck = TRUE)

save.image(file = ".RData", version = NULL, ascii = FALSE,
           compress = !ascii, safe = TRUE)
}
\arguments{
  \item{\dots}{the names of the objects to be saved (as symbols or
    character strings).}
  \item{list}{A character vector containing the names of objects to be
    saved.}
  \item{file}{a (writable binary-mode) \link{connection} or the name of the
    file where the data will be saved (when \link{tilde expansion}
    is done).  Must be a file name for \code{save.image} or
    \code{version = 1}.}
  \item{ascii}{if \code{TRUE}, an ASCII representation of the data is
    written.  The default value of \code{ascii} is \code{FALSE} which
    leads to a binary file being written.  If \code{NA} and
    \code{version >= 2}, a different ASCII representation is used which
    writes double/complex numbers as binary fractions.}
  \item{version}{the workspace format version to use.  \code{NULL}
    specifies the current default format.  The version used from \R
    0.99.0 to \R 1.3.1 was version 1.  The default format as from \R
    1.4.0 is version 2.}
  \item{envir}{environment to search for objects to be saved.}
  \item{compress}{logical or character string specifying whether saving
    to a named file is to use compression.  \code{TRUE} corresponds to
    \command{gzip} compression, and character strings \code{"gzip"},
    \code{"bzip2"} or \code{"xz"} specify the type of
    compression.  Ignored when \code{file} is a connection and
    for workspace format version 1.}
  \item{compression_level}{integer: the level of compression to be
    used.  Defaults to \code{6} for \command{gzip} compression and to
    \code{9} for \command{bzip2} or \command{xz} compression.}
  \item{eval.promises}{logical: should objects which are promises be
    forced before saving?}
  \item{precheck}{logical: should the existence of the objects be
    checked before starting to save (and in particular before opening
    the file/connection)?  Does not apply to version 1 saves.}
  \item{safe}{logical.  If \code{TRUE}, a temporary file is used for
    creating the saved workspace.  The temporary file is renamed to
    \code{file} if the save succeeds.  This preserves an existing
    workspace \code{file} if the save fails, but at the cost of using
    extra disk space during the save.}
}
\details{
  The names of the objects specified either as symbols (or character
  strings) in \code{\dots} or as a character vector in \code{list} are
  used to look up the objects from environment \code{envir}.  By default
  \link{promises} are evaluated, but if \code{eval.promises = FALSE}
  promises are saved (together with their evaluation environments).
  (Promises embedded in objects are always saved unevaluated.)

  All \R platforms use the XDR (bigendian) representation of C ints and
  doubles in binary save-d files, and these are portable across all \R
  platforms.

  ASCII saves used to be useful for moving data between platforms but
  are now mainly of historical interest.  They can be more compact than
  binary saves where compression is not used, but are almost always
  slower to both read and write: binary saves compress much better than
  ASCII ones.  Further, decimal ASCII saves may not restore
  double/complex values exactly, and what value is restored may depend
  on the \R platform.

  Default values for the \code{ascii}, \code{compress}, \code{safe} and
  \code{version} arguments can be modified with the
  \code{"save.defaults"} option (used both by \code{save} and
  \code{save.image}), see also the \sQuote{Examples} section.  If a
  \code{"save.image.defaults"} option is set it is used in preference to
  \code{"save.defaults"} for function \code{save.image} (which allows
  this to have different defaults).  In addition,
  \code{compression_level} can be part of the \code{"save.defaults"}
  option.

  A connection that is not already open will be opened in mode
  \code{"wb"}.  Supplying a connection which is open and not in binary
  mode gives an error.
}

\section{Compression}{
  Large files can be reduced considerably in size by compression.  A
  particular 46MB \R object was saved as 35MB without compression in 2
  seconds, 22MB with \command{gzip} compression in 8 secs, 19MB with
  \command{bzip2} compression in 13 secs and 9.4MB with \command{xz}
  compression in 40 secs.  The load times were 1.3, 2.8, 5.5 and 5.7
  seconds respectively.  These results are indicative, but the relative
  performances do depend on the actual file: \command{xz} compressed
  unusually well here.

  It is possible to compress later (with \command{gzip}, \command{bzip2}
  or \command{xz}) a file saved with \code{compress = FALSE}: the effect
  is the same as saving with compression.  Also, a saved file can be
  uncompressed and re-compressed under a different compression scheme
  (and see \code{\link{resaveRdaFiles}} for a way to do so from within \R).
}

\note{
  The most common reason for failure is lack of write permission in the
  current directory.  For \code{save.image} and for saving at the end of
  a session this will shown by messages like
\preformatted{    Error in gzfile(file, "wb") : unable to open connection
    In addition: Warning message:
    In gzfile(file, "wb") :
      cannot open compressed file '.RDataTmp',
      probable reason 'Permission denied'
}
#ifdef windows

  \code{file} can be a UTF-8-encoded filepath that cannot be translated to
  the current locale.
#endif
}

\section{Warnings}{
  The \code{\dots} arguments only give the \emph{names} of the objects
  to be saved: they are searched for in the environment given by the
  \code{envir} argument, and the actual objects given as arguments need
  not be those found.

  Saved \R objects are binary files, even those saved with
  \code{ascii = TRUE}, so ensure that they are transferred without
  conversion of end-of-line markers and of 8-bit characters.  The lines
  are delimited by LF on all platforms.

  Although the default version has not changed since \R 1.4.0, this
  does not mean that saved files are necessarily backwards compatible.
  You will be able to load a saved image into an earlier version of \R
  unless use is made of later additions (for example, raw vectors,
  external pointers and some S4 objects).

  One such \sQuote{later addition} was long vectors, introduced in \R
  3.0.0 and loadable only on 64-bit platforms.

  Loading files saved with \code{ASCII = NA} requires a C99-compliant C
  function \code{sscanf}: this is a problem on Windows, first worked
  around in \R 3.1.2: they should be readable in earlier versions of \R
  on all other platforms.
}
\seealso{
  \code{\link{dput}}, \code{\link{dump}}, \code{\link{load}},
  \code{\link{data}}.

  For other interfaces to the underlying serialization format, see
  \code{\link{serialize}} and \code{\link{saveRDS}}.
}
\examples{
x <- stats::runif(20)
y <- list(a = 1, b = TRUE, c = "oops")
save(x, y, file = "xy.RData")
save.image()
unlink("xy.RData")
unlink(".RData")

# set save defaults using option:
options(save.defaults = list(ascii = TRUE, safe = FALSE))
save.image()
unlink(".RData")
}
\keyword{file}
