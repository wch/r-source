% File src/library/base/man/file.info.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{file.info}
\alias{file.info}
\alias{file.mode}
\alias{file.mtime}
\alias{file.size}
\title{Extract File Information}
\description{
  Utility function to extract information about files on the user's
  file systems.
}
\usage{
file.info(\dots, extra_cols = TRUE)

file.mode(\dots)
file.mtime(\dots)
file.size(\dots)
}
\arguments{
  \item{\dots}{character vectors containing file paths.  Tilde-expansion
    is done: see \code{\link{path.expand}}.}
  \item{extra_cols}{Logical: return all cols rather than just the
    first six.}
}
\details{
  What constitutes a \sQuote{file} is OS-dependent but includes
  directories.  (However, directory names must not include a trailing
  backslash or slash on Windows.)  See also the section in the help for
  \code{\link{file.exists}} on case-insensitive file systems.

  The file \sQuote{mode} follows POSIX conventions, giving three octal
  digits summarizing the permissions for the file owner, the owner's
  group and for anyone respectively.  Each digit is the logical
  \emph{or} of read (4), write (2) and execute/search (1) permissions.

#ifdef unix
  On most systems symbolic links are followed, so information is given
  about the file to which the link points rather than about the link.
#endif
#ifdef windows
  File modes are probably only useful on NTFS file systems, and it seems
  all three digits refer to the file's owner.
  The execute/search bits are set for directories, and for files based
  on their extensions (e.g., \file{.exe}, \file{.com}, \file{.cmd}
  and \file{.bat} files).  \code{\link{file.access}} will give a more
  reliable view of read/write access availability to the \R process.

  UTF-8-encoded file names not valid in the current locale can be used.

  Junction points and symbolic links are followed, so information is
  given about the file/directory to which the link points rather than
  about the link.
#endif
}
\value{
  For \code{file.info}, data frame with row names the file names and columns
  \item{size}{double: File size in bytes.}
  \item{isdir}{logical: Is the file a directory?}
  \item{mode}{integer of class \code{"octmode"}.  The file permissions,
    printed in octal, for example \code{644}.}
  \item{mtime, ctime, atime}{integer of class \code{"POSIXct"}:
    file modification, \sQuote{last status change} and last access times.}
#ifdef unix
  \item{uid}{integer: the user ID of the file's owner.}
  \item{gid}{integer: the group ID of the file's group.}
  \item{uname}{character: \code{uid} interpreted as a user name.}
  \item{grname}{character: \code{gid} interpreted as a group name.}
  Unknown user and group names will be \code{NA}.
#endif
#ifdef windows
  \item{exe}{character: what sort of executable is this?  Possible
    values are \code{"no"}, \code{"msdos"}, \code{"win16"},
    \code{"win32"}, \code{"win64"} and \code{"unknown"}.  Note that a
    file (e.g., a script file) can be executable according to the mode
    bits but not executable in this sense.}
#endif

  If \code{extra_cols} is false, only the first six columns are
  returned: as these can all be found from a single C system call this
  can be faster.  (However, properly configured systems will use a
  \sQuote{name service cache daemon} to speed up the name lookups.)
  
  Entries for non-existent or non-readable files will be \code{NA}.
#ifdef unix
  The \code{uid}, \code{gid}, \code{uname} and \code{grname} columns
  may not be supplied on a non-POSIX Unix-alike system, and will not be
  on Windows.
#endif

  What is meant by the three file times depends on the OS and file
  system.  On Windows native file systems \code{ctime} is the file
  creation time (something which is not recorded on most Unix-alike file
  systems).  What is meant by \sQuote{file access} and hence the
  \sQuote{last access time} is system-dependent.

  The times are reported to an accuracy of seconds, and perhaps more on
  some systems.  However, many file systems only record times in
  seconds, and some (e.g., modification time on FAT systems) are recorded
  in increments of 2 or more seconds.
  
  \code{file.mode}, \code{file.mtime} and \code{file.size} are
  convenience wrappers returning just one of the columns.
}
#ifdef unix
\note{
  Some systems allow files of more than 2Gb to be created but
  not accessed by the \code{stat} system call.  Such files will show up
  as non-readable (and very likely not be readable by any of \R's input
  functions) -- fortunately such file systems are becoming rare.
}
#endif

\seealso{
  \code{\link{Sys.readlink}} to find out about symbolic links,
  \code{\link{files}}, \code{\link{file.access}},
  \code{\link{list.files}},
  and \code{\link{DateTimeClasses}} for the date formats.

  \code{\link{Sys.chmod}} to change permissions.
}
\examples{
ncol(finf <- file.info(dir()))  # at least six
\donttest{finf # the whole list}
## Those that are more than 100 days old :
finf <- file.info(dir(), extra_cols = FALSE)
finf[difftime(Sys.time(), finf[,"mtime"], units = "days") > 100 , 1:4]

file.info("no-such-file-exists")
}
\keyword{file}
