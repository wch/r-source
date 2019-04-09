\name{update_PACKAGES}
\alias{update_PACKAGES}
\title{Update Existing PACKAGES Files}
\usage{
update_PACKAGES(dir = ".", fields = NULL, type = c("source",
  "mac.binary", "win.binary"), verbose.level = as.integer(dryrun),
  latestOnly = TRUE, addFiles = FALSE, rds_compress = "xz",
  strict = TRUE, dryrun = FALSE)
}
\arguments{
\item{dir}{See \code{\link{write_PACKAGES}}}

\item{fields}{See \code{\link{write_PACKAGES}}}

\item{type}{See \code{\link{write_PACKAGES}}}

\item{verbose.level}{(0, 1,  2) What level of informative messages
which should be displayed throughout the process. Defaults to 0 if
\code{dryrun}   is   \code{FALSE}    (the   default)   and   1
otherwise. See details for more information.}

\item{latestOnly}{See \code{\link{write_PACKAGES}}}

\item{addFiles}{See \code{\link{write_PACKAGES}}}

\item{rds_compress}{See \code{\link{write_PACKAGES}}}

\item{strict}{logical. Should 'strict mode' be used when checking
existing \code{PACKAGES} entries. See details. Defaults to
\code{TRUE}.}

\item{dryrun}{logical. Should the updates to existing \code{PACKAGES}
  files be computed but NOT applied. Defaults to \code{FALSE}.}

}
\description{
Update an existing repository by reading the \code{PACKAGES}
file, retaining entries which are still valid, removing entries which
are no longer valid, and only processing built package tarballs which do not
match existing entries.

\code{update_PACKAGES} can be much faster than
\code{\link{write_PACKAGES}} for small-moderate changes to large
repository indexes, particularly in non-strict mode (see Details).
}
\details{
Throughout this section, \emph{package tarball} is defined to mean any
archive file in \code{dir} whose name can be interpreted as
\code{<package>_<version>.<ext>} - with \code{<ext>} the appropriate
extension for built packages of type \code{type} - (or that is pointed
to by the \code{File} field of an existing \code{PACKAGES} entry).
\emph{Novel package tarballs} are those which do not match an existing
\code{PACKAGES} file entry.

\code{update_PACKAGES} calls directly down to
\code{\link{write_PACKAGES}} with a warning (and thus all package tarballs
will be processed), if any of the following conditions hold:
\itemize{
  \item \code{type} is \code{win.binary} and \code{strict} is
  \code{TRUE} (no MD5 checksums are included in win.binary \code{PACKAGES} files)
  \item No \code{PACKAGES} file exists under \code{dir}
  \item A \code{PACKAGES} file exists under \code{dir} but is empty 
  \item \code{fields} is not \code{NULL} and one or more specified fields
  are not present in the existing \code{PACKAGES} file
 }

\code{update_PACKAGES} avoids (re)processing package tarballs in cases where
a \code{PACKAGES} file entry already exists and appears to remain valid. The
logic for detecting still-valid entries is as follows:

Any package tarball which was last modified more recently than the
existing \code{PACKAGES} file is considered novel; existing
\code{PACKAGES} entries appearing to correspond to such tarballs are
\emph{always} considered stale and replaced by newly generated ones.
Similarly, all \code{PACKAGES} entries that do not correspond to any
package tarball found in \code{dir} are considered invalid and are
excluded from the resulting updated \code{PACKAGES} files.

When \code{strict} is \code{TRUE}, \code{PACKAGES} entries that match a package
tarball (by package name and version) are confirmed via MD5 checksum;
only those that pass are retained as valid. All novel package tarballs
are fully processed by the standard machinery underlying
\code{\link{write_PACKAGES}} and the resulting entries are
added. Finally, if \code{latestOnly} is \code{TRUE}, package-version
pruning is performed across the entries.

When \code{strict} is \code{FALSE}, package tarballs are assumed to encode
correct metadata in their filenames. \code{PACKAGES} entries which
appear to match a package tarball are retained as valid (No MD5 checksum
testing occurs). If \code{latestOnly} is \code{TRUE}, package-version
pruning is performed across the full set of retained entries and novel package
tarballs \emph{before} the processing of the novel tarballs, at
significant computational and time savings in some
situations. After the optional pruning, any relevant novel package
tarballs are processed via the standard machinery and added to the
set of retained entries.

In both cases, after the above process concludes, entries are sorted
alphabetically by the string concatenation of \code{Package} and
\code{Version}. This should match the entry order \code{write_PACKAGES}
outputs.

The fields within the entries are ordered as follows: canonical fields -
i.e., those appearing as columns when \code{available.packages} is
called on a CRAN mirror - appear first in their canonical order,
followed by any non-canonical fields.

After entry and field reordering, the final database of \code{PACKAGES}
entries is written to all three \code{PACKAGES} files, overwriting the
existing versions.

When \code{verbose.level} is \code{0}, no extra messages are displayed
to the user. When it is \code{1}, detailed information about what is
happening is conveyed via messages, but underlying machinery from
\code{\link{write_PACKAGES}} is invoked with \code{verbose = FALSE}.
Behavior when \code{verbose.level} is \code{2} is identical to
\code{verbose.level} \code{1} with the exception that underlying
machinery from \code{write_PACKAGE} is invoked with
\code{verbose = TRUE}, which will individually list every processed
tarball.
}
\note{
While both strict and non-strict modes can offer speedups when updating
small percentages of large repositories, non-strict mode is \emph{much}
faster and is recommended in situations where the assumption it makes
about tarballs' filenames encoding accurate information is safe.  }
\note{ Users should expect significantly smaller speedups over
\code{write_PACKAGES} in the \code{type == "win.binary"} case on at
least some operating systems. This is due to \code{write_PACKAGES} being
significantly faster in this context, rather than \code{update_PACKAGES}
being slower.
}
\seealso{
  \link{write_PACKAGES}
} 
\author{
  Gabriel Becker (adapted from previous, related work by him in the \code{switchr}
  package which is copyright Genentech, Inc.)
}
\examples{
\dontrun{
write_PACKAGES("c:/myFolder/myRepository") # on Windows
update_PACKAGES("c:/myFolder/myRepository") # on Windows
write_PACKAGES("/pub/RWin/bin/windows/contrib/2.9",
type = "win.binary") # on Linux
update_PACKAGES("/pub/RWin/bin/windows/contrib/2.9",
type = "win.binary") # on Linux
}}
\keyword{file}
\keyword{utilities}

