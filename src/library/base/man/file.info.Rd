\name{file.info}
\alias{file.info}
\title{Extract File Information}
\description{
  Utility function to extract information about files on the user's
  file systems.
}
\usage{
file.info(...)
}
\arguments{
  \item{\dots}{character vectors containing file names.}
}
\details{
  The file times given by this function are in days and partial days
  since 1970-1-1.  These will be interpreted by the \code{chron}
  function in package \code{chron} as days and times in the UTC
  time zone, that is ignoring any timezone information.

#ifdef windows
  File modes are probably only useful on Windows NT machines.
#endif
}
\value{
  A data frame with row names the file names and columns
  \item{size}{integer: File size in bytes.}
  \item{isdir}{logical: Is the file a directory?}
  \item{mode}{integer of class \code{"octmode"}.  The file permissions,
    printed in octal, for example \code{644}.}
  \item{mtime, ctime, atime}{real: file modification, creation and last
    access times in days since 1970-1-1 in the UTC timezone.}
#ifdef unix
  \item{uid}{integer: the user ID of the file's owner.}
  \item{gid}{integer: the group ID of the file's group.}
  \item{uname}{character: \code{uid} interpreted as a user name.}
  \item{grname}{character: \code{gid} interpreted as a group name.}
  Unknown user and group names will be \code{NA}.
#endif

Entries for non-existent or non-readable files will be \code{NA}.
#ifdef unix
The \code{uid}}, \code{gid}, \code{uname} and \code{grname} columns
may not be supplied on a non-POSIX Unix system.
#endif
}

\author{B. D. Ripley}

\note{
  This function will only be operational on systems with the
  \code{stat} system call, but that seems very widely available.

  The internal function returns times in the Unix format of seconds
  since 1970-1-1 should that ever be required.

  What is meant by file access time is system-dependent.
}
\section{Warning}{
  The format and handling of file dates will be changed before release.
}
\seealso{
  \code{\link{file}}, \code{\link{file.access}}, \code{\link{list.files}}
}
\examples{
%-- the output of this should be "diff"able
ncol(finf <- file.info(dir()))# at least six
\dontrun{finf # the whole list}
## Those that are more than 100 days old :
finf[ (Sys.time() - finf[,"mtime"]) > (24*60^2) * 100 , 1:4]

file.info("no-such-file-exists")
}
\keyword{file}
