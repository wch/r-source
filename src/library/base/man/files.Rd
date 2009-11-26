% File src/library/base/man/files.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{files}
\alias{files}
\alias{file.append}
\alias{file.copy}
\alias{file.create}
\alias{file.exists}
\alias{file.rcopy}
\alias{file.remove}
\alias{file.rename}
\alias{file.symlink}
\title{File Manipulation}
\usage{
file.create(\dots, showWarnings = TRUE)
file.exists(\dots)
file.remove(\dots)
file.rename(from, to)
file.append(file1, file2)
file.copy(from, to, overwrite = recursive, recursive = FALSE)
file.symlink(from, to)
}
\arguments{
  \item{\dots, file1, file2, from, to}{character vectors,
    containing file names or paths.}
  \item{overwrite}{logical; should the destination files be overwritten?}
  \item{showWarnings}{logical; should the warnings on failure be shown?}
  \item{recursive}{logical. If \code{to} is a directory, should
    directories in \code{from} be copied (and their contents).}
}
\description{
  These functions provide a low-level interface to the computer's
  file system.
}
\details{
  The \code{\dots} arguments are concatenated to form one character
  string: you can specify the files separately or as one vector.
  All of these functions expand path names: see \code{\link{path.expand}}.

  \code{file.create} creates files with the given names if they do not
  already exist and truncates them if they do.  They are created with the
  maximal permissions allowed by the \code{umask} setting.

  \code{file.exists} returns a logical vector indicating whether
  the files named by its argument exist.  (Here \sQuote{exists} is in the
  sense of the system's \code{stat} call: a file will be reported as
  existing only if you have the permissions needed by \code{stat}.
  Existence can also be checked by \code{\link{file.access}}, which
  might use different permissions and so obtain a different result.
  Note that the existence of a file does not imply that it is readable:
  for that use \code{\link{file.access}}.)
#ifdef unix
  Note that if the file is a symbolic link, the result indicates if the
  link points to an actual file, not just if the link exists.
#endif

  \code{file.remove} attempts to remove the files named in its argument.
  On most Unix platforms \sQuote{file} includes \emph{empty}
  directories, symbolic links, fifos and sockets.  On Windows,
  \sQuote{file} means a regular file and not, say, an empty directory.

  
  \code{file.rename} attempts to rename a single file.  Where file
  permissions allow this will first remove an existing file \code{to}.
  This is subject to the limitations of the OS's corresponding system
  call: in particular in the interpretation of \sQuote{file}: also most
  platforms will not rename files across file systems.

  \code{file.append} attempts to append the files named by its
  second argument to those named by its first.  The \R subscript
  recycling rule is used to align names given in vectors
  of different lengths.

  \code{file.copy} works in a similar way to \code{file.append} but with
  the arguments in the natural order for copying.  Copying to existing
  destination files is skipped unless \code{overwrite = TRUE}.
  The \code{to} argument can specify a single existing directory.

  \code{file.symlink} makes symbolic links on those Unix-like platforms
  which support them.  The \code{to} argument can specify a single
  existing directory.
#ifdef windows
  (Windows Vista/Server 2008 have a version of symbolic links on NTFS
  filesystems, but this seems too limited to be worth implementing.)
#endif
}
\value{
  \code{file.rename} returns a logical value, true for success.

  The remaining functions return a logical vector indicating which
  operation succeeded for each of the files attempted.  Using a missing
  value for a file or path name will always be regarded as a failure.

  If \code{showWarnings = TRUE}, \code{file.create} will give a warning
  for an unexpected failure.
}
#ifdef windows
\note{
  There is no guarantee that these functions will handle Windows
  relative paths of the form \file{d:path}: try \file{d:./path}
  instead.  In particular, \file{d:} is not recognized as a directory.
}
#endif

\author{
  Ross Ihaka, Brian Ripley
}
\seealso{
  \code{\link{file.info}}, \code{\link{file.access}}, \code{\link{file.path}},
  \code{\link{file.show}}, \code{\link{list.files}},
  \code{\link{unlink}}, \code{\link{basename}},
  \code{\link{path.expand}}.

  \code{\link{dir.create}}.

  \code{\link{Sys.glob}} to expand wildcards in file specifications.
  
  \code{\link{file_test}}, \code{\link{Sys.readlink}}.
}

\examples{
cat("file A\n", file="A")
cat("file B\n", file="B")
file.append("A", "B")
file.create("A")
file.append("A", rep("B", 10))
if(interactive()) file.show("A")
file.copy("A", "C")
dir.create("tmp")
file.copy(c("A", "B"), "tmp")
list.files("tmp")
#ifdef unix
setwd("tmp")
file.remove("B")
file.symlink(file.path("..", c("A", "B")), ".")
setwd("..")
#endif
unlink("tmp", recursive=TRUE)
file.remove("A", "B", "C")
}
\keyword{file}
