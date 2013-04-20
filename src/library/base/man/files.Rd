% File src/library/base/man/files.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{files}
\alias{files}
\alias{file.append}
\alias{file.copy}
\alias{file.create}
\alias{file.exists}
\alias{file.remove}
\alias{file.rename}
\alias{file.symlink}
\alias{file.link}
#ifdef windows
\alias{Sys.junction}
#endif

\title{File Manipulation}
\usage{
file.create(\dots, showWarnings = TRUE)
file.exists(\dots)
file.remove(\dots)
file.rename(from, to)
file.append(file1, file2)
file.copy(from, to, overwrite = recursive, recursive = FALSE,
          copy.mode = TRUE)
file.symlink(from, to)
file.link(from, to)
#ifdef windows
Sys.junction(from, to)
#endif
}
\arguments{
  \item{\dots, file1, file2}{character vectors, containing file names or paths.}
  \item{from, to}{character vectors, containing file names or paths.
    For \code{file.copy} and \code{file.symlink}
#ifdef windows
    and \code{Sys.junction}
#endif
    \code{to} can alternatively be the path to a single existing directory.}
  \item{overwrite}{logical; should existing destination files be overwritten?}
  \item{showWarnings}{logical; should the warnings on failure be shown?}
  \item{recursive}{logical.  If \code{to} is a directory, should
    directories in \code{from} be copied (and their contents)?  (Like
    \command{cp -R} on POSIX OSes.)}
  \item{copy.mode}{logical:  should file permission bits be copied where
    possible?  This applies to both files and directories.}
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
  already exist and truncates them if they do.  They are created with
  the maximal read/write permissions allowed by the
  \sQuote{\link{umask}} setting (where relevant).  By default a warning
  is given (with the reason) if the operation fails.

  \code{file.exists} returns a logical vector indicating whether the
  files named by its argument exist.  (Here \sQuote{exists} is in the
  sense of the system's \code{stat} call: a file will be reported as
  existing only if you have the permissions needed by \code{stat}.
  Existence can also be checked by \code{\link{file.access}}, which
  might use different permissions and so obtain a different result.
  Note that the existence of a file does not imply that it is readable:
  for that use \code{\link{file.access}}.)  What constitutes a
  \sQuote{file} is system-dependent, but should include directories.
  (However, directory names must not include a trailing backslash or
  slash on Windows.)  Note that if the file is a symbolic link on a
  Unix-alike, the result indicates if the link points to an actual file,
  not just if the link exists.
  Lastly, note the \emph{different} function \code{\link{exists}} which
  checks for existence of \R objects.

  \code{file.remove} attempts to remove the files named in its argument.
  On most Unix platforms \sQuote{file} includes \emph{empty}
  directories, symbolic links, fifos and sockets.  On Windows,
  \sQuote{file} means a regular file and not, say, an empty directory.

  \code{file.rename} attempts to rename files (and \code{from} and
  \code{to} must be of the same length).  Where file permissions allow
  this will overwrite an existing element of \code{to}.  This is subject
  to the limitations of the OS's corresponding system call (see
  something like \command{man 2 rename} on a Unix-alike): in particular
  in the interpretation of \sQuote{file}: most platforms will not rename
  files across file systems.  (On Windows, \code{file.rename} can move
  files but not directories between volumes.)  On platforms which allow
  directories to be renamed, typically neither or both of \code{from}
  and \code{to} must a directory, and if \code{to} exists it must be an
  empty directory.

  \code{file.append} attempts to append the files named by its
  second argument to those named by its first.  The \R subscript
  recycling rule is used to align names given in vectors
  of different lengths.

  \code{file.copy} works in a similar way to \code{file.append} but with
  the arguments in the natural order for copying.  Copying to existing
  destination files is skipped unless \code{overwrite = TRUE}.  The
  \code{to} argument can specify a single existing directory.  If
  \code{copy.mode = TRUE} (added in \R 2.13.0) file read/write/execute
  permissions are copied where possible, restricted by
  \sQuote{\link{umask}}.  Other security attributes such as ACLs are not
  copied.  On a POSIX filesystem the targets of symbolic links will be
  copied rather than the links themselves, and hard links are copied
  separately.

  \code{file.symlink} and \code{file.link} make symbolic and hard links
  on those file systems which support them.  For \code{file.symlink} the
  \code{to} argument can specify a single existing directory.  (Unix and
  OS X native filesystems support both.  Windows has hard links to
  files on NTFS file systems and concepts related to symbolic links on
  recent versions: see the section below on the Windows version of this
  help page.  What happens on a FAT or SMB-mounted file system is OS-specific.)
}

\value{
  These functions return a logical vector indicating which
  operation succeeded for each of the files attempted.  Using a missing
  value for a file or path name will always be regarded as a failure.

  If \code{showWarnings = TRUE}, \code{file.create} will give a warning
  for an unexpected failure.
}

\section{Case-insensitive file systems}{
  Case-insensitive file systems are the norm on Windows and OS X,
  but can be found on all OSes (for example a FAT-formatted USB drive is
  probably case-insensitive).

  These functions will most likely match existing files regardless of case
  on such file systems: however this is an OS function and it is
  possible that file names might be mapped to upper or lower case.
}

#ifdef windows
\note{
  There is no guarantee that these functions will handle Windows
  relative paths of the form \file{d:path}: try \file{d:./path}
  instead.  In particular, \file{d:} is not recognized as a directory.
  Nor are \samp{\\\\?\\} prefixes (and similar) supported.

  Most of these functions accept UTF-8 filepaths not valid in the
  current locale.

  User error in supplying invalid file names (and note that \file{foo/}
  and \file{foo\\} \emph{are} invalid on Windows) has undefined consequences.
}

\section{Symbolic links on Windows}{ Symbolic links in the sense of
  POSIX file systems do not exist on Windows: however, NTFS file systems
  support two similar concepts.

  Windows 2000 and later have \sQuote{junctions} (or
  \sQuote{junction points}), unfortunately without a public API.  They
  are a Windows version of the Unix concept of mounting one directory on
  another.  One way to create, list and delete junctions is \emph{via}
  \file{junction.exe} from
  \url{http://download.sysinternals.com/Files/Junction.zip} (see
  \url{http://technet.microsoft.com/en-us/sysinternals/bb896768}).  On
  recent enough versions of Windows \command{mklink /J} can also be
  used.  Function \code{Sys.junction} (added in \R 2.15.0) creates one
  or more junctions: \code{to} should either specify a single existing
  directory or a set of non-existent file paths of the same length as
  \code{from}.

  A version of symbolic linking to files/directories was implemented
  starting with Vista, and \code{file.symlink} makes use of that
  interface since \R 2.15.0.  However, it has restrictions (apart from
  the OS version restriction) which are crippling.  First, the user
  needs permission to make symbolic links, and that permission is not
  normally granted except to Administrator accounts (note: not users
  with Administrator rights): further many users report that whereas the
  Policy Editor appears to be able to grant such rights, the API still
  reports insufficient permissions.  Second, the interface needs to know
  if \code{from} is a file or a directory (and it need not yet exist):
  we have implemented this to allow linking from a directory only if it
  currently exists.

  Care is needed with removing a junction (and most likely also a
  symbolic link): many tools will remove the target and its contents
  (including Windows Explorer in XP, and \code{unlink} in \R prior to
  2.15.0).
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

  \url{http://en.wikipedia.org/wiki/Hard_link} and
  \url{http://en.wikipedia.org/wiki/Symbolic_link} for the concepts of
  links and their limitations.
}

\examples{
cat("file A\n", file = "A")
cat("file B\n", file = "B")
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
unlink("tmp", recursive = TRUE)
file.remove("A", "B", "C")
}
\keyword{file}
