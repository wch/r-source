% File src/library/base/man/files.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{files}
\alias{files}
\concept{directory}
\title{File and Directory Manipulation}
\usage{
file.create(\dots)
file.exists(\dots)
file.remove(\dots)
file.rename(from, to)
file.append(file1, file2)
file.copy(from, to, overwrite = FALSE)
file.symlink(from, to)
dir.create(path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
Sys.chmod(paths, mode = "0777")
}
\alias{file.append}
\alias{file.copy}
\alias{file.create}
\alias{file.exists}
\alias{file.remove}
\alias{file.rename}
\alias{file.symlink}
\alias{dir.create}
\alias{Sys.chmod}
\arguments{
  \item{\dots, file1, file2, from, to, paths}{character vectors,
    containing file names or paths.}
  \item{path}{a character vector containing a single path name.}
  \item{overwrite}{logical; should the destination files be overwritten?}
  \item{showWarnings}{logical; should the warnings on failure be shown?}
  \item{recursive}{logical: should elements of the path other than the
    last be created?  If true, like Unix's \code{mkdir -p}.}
  \item{mode}{the file mode to be used on Unix-alikes: it will be
    coerced by \code{\link{as.octmode}}.}
}
\description{
  These functions provide a low-level interface to the computer's
  file system.
}
\details{
  The \code{\dots} arguments are concatenated to form one character
  string: you can specify the files separately or as one vector.
  All of these functions expand path names: see \code{\link{path.expand}}.

  \code{file.create} creates files with the given names if they
  do not already exist and truncates them if they do.  On a Unix-alike
  they are created with the maximal permissions allowed by the
  \code{umask} setting.

  \code{file.exists} returns a logical vector indicating whether
  the files named by its argument exist.  (Here \sQuote{exists} is in the
  sense of the system's \code{stat} call: a file will be reported as
  existing only if you have the permissions needed by \code{stat}.
  Existence can also be checked by \code{\link{file.access}}, which
  might use different permissions and so obtain a different result.
  Note that the existence of a file does not imply that it is readable:
  for that use \code{\link{file.access}}.)

  \code{file.remove} attempts to remove the files named in its
  argument.

  \code{file.rename} attempts to rename a single file.
#ifdef windows
  On Windows 9x/ME rename is not atomic, so it is possible that
  \code{to} will be deleted but \code{from} will not be renamed.
#endif

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
  
  \code{dir.create} creates the last element of the path, unless
  \code{recursive = TRUE}.  Trailing path separators are removed.
#ifdef windows
  On Windows drives are allowed in the path specification and unless
  the path is rooted, it will be interpreted relative to the current
  directory on that drive.  \code{mode} is ignored on Windows.
#endif
#ifdef unix
  The mode will be modified by the \code{umask} setting in the same way
  as for the system function \code{mkdir}.  What modes can be set is
  OS-dependent, and it is unsafe to assume that more than three octal
  digits will be used.  For more details see your OS's documentation on the
  system call \code{mkdir} (and not that on the command-line utility of
  that name). 
#endif

  \code{Sys.chmod} sets the file permissions of one or more files.
#ifdef unix
  It may not be supported (when a warning is issued).
  See the comments for \code{dir.create} for how modes are interpreted.
  Changing mode on a symbolic link is unlikely to work (nor be
  necessary).  For more details see your OS's documentation on the
  system call \code{chmod} (and not that on the command-line utility of
  that name). 
#endif
#ifdef windows
  The interpretation of \code{mode} in the Windows system function is
  non-POSIX and only supports setting the read-only attribute of the
  file.  So \R interprets \code{mode} to mean set read-only if and only
  if \code{mode & 0200 == 0} (interpreted in octal).  Windows has a much
  more extensive system of file permissions on some file systems
  (e.g. versions of NTFS) which are unrelated to this system call.
#endif
}
\value{
  \code{dir.create} and \code{file.rename} return a logical,
  true for success.

  The remaining functions return a logical vector indicating which
  operation succeeded for each of the files attempted.

  \code{dir.create} will return failure if the directory already exists. 
}
\author{
  Ross Ihaka, Brian Ripley
}
\seealso{
  \code{\link{file.info}}, \code{\link{file.access}}, \code{\link{file.path}},
  \code{\link{file.show}}, \code{\link{list.files}},
  \code{\link{unlink}}, \code{\link{basename}}, \code{\link{path.expand}}.

  \code{\link{file_test}}.
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
