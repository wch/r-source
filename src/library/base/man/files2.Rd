% File src/library/base/man/files.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{files2}
\alias{dir.create}
\alias{dir.exists}
\alias{Sys.chmod}
\alias{Sys.umask}
\concept{directory}
\concept{mkdir}
\alias{umask} % for links
\title{Manipulation of Directories and File Permissions}
\description{
  These functions provide a low-level interface to the computer's
  file system.
}
\usage{
dir.exists(paths)
dir.create(path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
Sys.chmod(paths, mode = "0777", use_umask = TRUE)
Sys.umask(mode = NA)
}
\arguments{
  \item{path}{a character vector containing a single path name.  Tilde
    expansion (see \code{\link{path.expand}}) is done.}
  \item{paths}{character vectors containing file or directory paths.  Tilde
    expansion (see \code{\link{path.expand}}) is done.}
  \item{showWarnings}{logical; should the warnings on failure be shown?}
  \item{recursive}{logical. Should elements of the path other than the
    last be created?  If true, like the Unix command \command{mkdir -p}.}
  \item{mode}{the mode to be used on Unix-alikes: it will be
    coerced by \code{\link{as.octmode}}.  For \code{Sys.chmod} it is
    recycled along \code{paths}.}
  \item{use_umask}{logical: should the mode be restricted by the
    \code{umask} setting?}
}
\details{
  \code{dir.create} creates the last element of the path, unless
  \code{recursive = TRUE}.  Trailing path separators are discarded.
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
  system call \code{mkdir}, e.g.\sspace{}\command{man 2 mkdir} (and not that on
  the command-line utility of that name).
#endif

  One of the idiosyncrasies of Windows is that directory creation may
  report success but create a directory with a different name, for
  example \code{dir.create("G.S.")} creates \file{"G.S"}.  This is
  undocumented, and what are the precise circumstances is unknown (and
  might depend on the version of Windows).  Also avoid directory names
  with a trailing space.
  %% http://msdn.microsoft.com/en-us/library/aa365247%28VS.85%29.aspx
  %% is vague about this!

  \code{Sys.chmod} sets the file permissions of one or more files.
#ifdef unix
  It may not be supported on a system (when a warning is issued).
  See the comments for \code{dir.create} for how modes are interpreted.
  Changing mode on a symbolic link is unlikely to work (nor be
  necessary).  For more details see your OS's documentation on the
  system call \code{chmod}, e.g.\sspace{}\command{man 2 chmod} (and not that on
  the command-line utility of that name).  Whether this changes the
  permission of a symbolic link or its target is OS-dependent (although
  to change the target is more common, and POSIX does not support modes
  for symbolic links: BSD-based Unixes do, though).
#endif
#ifdef windows
  The interpretation of \code{mode} in the Windows system functions is
  non-POSIX and only supports setting the read-only attribute of the
  file.  So \R interprets \code{mode} to mean set read-only if and only
  if \code{(mode & 0200) == 0} (interpreted in octal).  Windows has a much
  more extensive system of file permissions on some file systems
  (e.g., versions of NTFS) which are unrelated to this system call.
#endif

  \code{Sys.umask} sets the \code{umask} and returns the previous value:
  as a special case \code{mode = NA} just returns the current value.
#ifdef unix
  It may not be supported (when a warning is issued and \code{"0"}
  is returned).  For more details see your OS's documentation on the
  system call \code{umask}, e.g.\sspace{}\command{man 2 umask}.
#endif
#ifdef windows
  All files on Windows are regarded as readable, and files being
  executable is not a Windows concept.  So \code{umask} only controls
  whether a file is writable: a setting of \code{"200"} makes files (but
  not directories) created subsequently read-only.
#endif
%% http://msdn.microsoft.com/en-us/library/5axxx3be%28v=vs.80%29.aspx

  How modes are handled depends on the file system, even on Unix-alikes
  (although their documentation is often written assuming a POSIX file
  system).  So treat documentation cautiously if you are using, say, a
  FAT/FAT32 or network-mounted file system.
}
\value{
  \code{dir.exists} returns a logical vector of \code{TRUE} or
  \code{FALSE} values (without names).
  
  \code{dir.create} and \code{Sys.chmod} return invisibly a logical vector
  indicating if the operation succeeded for each of the files attempted.
  Using a missing value for a path name will always be regarded as a
  failure.  \code{dir.create} indicates failure if the directory already
  exists.  If \code{showWarnings = TRUE}, \code{dir.create} will give a
  warning for an unexpected failure (e.g., not for a missing value nor
  for an already existing component for \code{recursive = TRUE}).

  \code{Sys.umask} returns the previous value of the \code{umask},
  as a length-one object of class \code{"\link{octmode}"}: the
  visibility flag is off unless \code{mode} is \code{NA}.

  See also the section in the help for \code{\link{file.exists}} on
  case-insensitive file systems for the interpretation of \code{path}
  and \code{paths}.
}
#ifdef windows
\note{
  There is no guarantee that these functions will handle Windows
  relative paths of the form \file{d:path}: try \file{d:./path}
  instead.  In particular, \file{d:} is not recognized as a directory.
  Nor are \samp{\\\\?\\} prefixes (and similar) supported.

  UTF-8-encoded dirnames not valid in the current locale can be used.
}
#endif

\author{
  Ross Ihaka, Brian Ripley
}
\seealso{
  \code{\link{file.info}}, \code{\link{file.exists}}, \code{\link{file.path}},
  \code{\link{list.files}}, \code{\link{unlink}},
  \code{\link{basename}}, \code{\link{path.expand}}.
}

\examples{\dontrun{
## Fix up maximal allowed permissions in a file tree
Sys.chmod(list.dirs("."), "777")
f <- list.files(".", all.files = TRUE, full.names = TRUE, recursive = TRUE)
Sys.chmod(f, (file.info(f)$mode | "664"))
}}
\keyword{file}
