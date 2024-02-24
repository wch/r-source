% File src/library/base/man/unlink.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2021 R Core Team
% Distributed under GPL 2 or later

\name{unlink}
\title{Delete Files and Directories}
\usage{unlink(x, recursive = FALSE, force = FALSE, expand = TRUE)}
\alias{unlink}
\arguments{
  \item{x}{a character vector with the names of the file(s) or
    directories to be deleted.}
  \item{recursive}{logical.  Should directories be deleted recursively?}
  \item{force}{logical.  Should permissions be changed (if possible) to
    allow the file or directory to be removed?}
  \item{expand}{logical. Should wildcards (see \sQuote{Details} below) and
    tilde (see \code{\link{path.expand}}) be expanded?}
}
\description{
  \code{unlink} deletes the file(s) or directories specified by \code{x}.
}
\details{
  If \code{recursive = FALSE} directories are not deleted,
  not even empty ones.

#ifdef unix
  On most platforms \sQuote{file} includes symbolic links, \I{fifo}s and
  sockets.  \code{unlink(x, recursive = TRUE)}
  deletes just the symbolic link if the target of such a link is a directory.

#endif
  Wildcard expansion (normally \file{*} and \file{?} are allowed) is done by
  the internal code of \code{\link{Sys.glob}}.  Wildcards never match a
  leading \file{.} in the filename, and files \file{.}, \file{..} and
  \file{~} will never be considered for deletion.
#ifdef unix
  Wildcards will only be expanded if the system supports it.  Most
  systems will support not only \file{*} and \file{?} but also character
  classes such as \file{[a-z]} (see the \command{man} pages for the system
  call \code{glob} on your OS).  The metacharacters \code{* ? [} can
  occur in Unix filenames, and this makes it difficult to use
  \code{unlink} to delete such files (see \code{\link{file.remove}}),
  although escaping the metacharacters by backslashes usually works.  If
  a metacharacter matches nothing it is considered as a literal
  character.

  \code{recursive = TRUE} might not be supported on all platforms, when it
  will be ignored, with a warning: however there are no known current
  examples.
#endif
#ifdef Windows
  Character classes such as \file{[a-z]} are supported.  The
  metacharacter \code{[} can occur in Windows filenames, and this makes
  it difficult to use \code{unlink} to delete such files (see
  \code{\link{file.remove}}).  If a wildcard matches
  nothing it is considered as a literal character.

  Windows cannot remove the current working directory, nor any file
  which is open nor any directory containing such a file.

  UTF-8-encoded paths not valid in the current locale can be used, but
  globbing will not work.
#endif
}
\value{
  \code{0} for success, \code{1} for failure, invisibly.
  Not deleting a non-existent file is not a failure, nor is being unable
  to delete a directory if \code{recursive = FALSE}.  However, missing
  values in \code{x} are regarded as failures.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{file.remove}}.
}
%Examples: tempfile has an 'unlink' example
\keyword{file}
