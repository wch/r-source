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
  What is meant by ``file access'' and hence the last access time is
  system-dependent.

#ifdef unix
  On most systems symbolic links are followed, so information is given
  about the file to which the link points rather than about the link.
#endif
#ifdef windows
  File modes are probably only useful on Windows NT/2000 machines.
#endif
}
\value{
  A data frame with row names the file names and columns
  \item{size}{integer: File size in bytes.}
  \item{isdir}{logical: Is the file a directory?}
  \item{mode}{integer of class \code{"octmode"}.  The file permissions,
    printed in octal, for example \code{644}.}
  \item{mtime, ctime, atime}{integer of class \code{"POSIXct"}:
    file modification, creation and last access times.}
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
\note{
  This function will only be operational on systems with the
  \code{stat} system call, but that seems very widely available.
}
\author{B. D. Ripley}

\seealso{
  \code{\link{files}}, \code{\link{file.access}},
  \code{\link{list.files}},
  and \code{\link{DateTimeClasses}} for the date formats.
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
