% File src/library/utils/man/changedFiles.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2013 R Core Team
% Distributed under GPL 2 or later

\name{changedFiles}
\alias{fileSnapshot}
\alias{changedFiles}
\alias{print.changedFiles}
\alias{print.fileSnapshot}
\title{
Detect which files have changed
}
\description{
\code{fileSnapshot} takes a snapshot of a selection of files,
recording summary information about each.  \code{changedFiles}
compares two snapshots, or compares one snapshot to the current state
of the file system.  The snapshots need not be the same directory;
this could be used to compare two directories.
}
\usage{
fileSnapshot(path = ".", file.info = TRUE, timestamp = NULL, 
	    md5sum = FALSE, digest = NULL, full.names = length(path) > 1,
	    ...) 

changedFiles(before, after, path = before$path, timestamp = before$timestamp, 
	    check.file.info = c("size", "isdir", "mode", "mtime"), 
	    md5sum = before$md5sum, digest = before$digest, 
	    full.names = before$full.names, ...)
	    
\S3method{print}{fileSnapshot}(x, verbose = FALSE, ...)

\S3method{print}{changedFiles}(x, verbose = FALSE, ...) 
} 

\arguments{ 

\item{path}{character vector; the path(s) to record. }
\item{file.info}{logical; whether to record \code{\link{file.info}}
values for each file.
}  

\item{timestamp}{character string or \code{NULL}; the name of a file
to write at the time the snapshot is taken.  This gives a quick test
for modification, but may be unreliable; see the Details. }

\item{md5sum}{logical; whether MD5 summaries of each file should be
taken as part of the snapshot. }

\item{digest}{a function or \code{NULL}; a function with header
\code{function(filename)} which will take a vector of filenames and
produce a vector of values of the same length, or a matrix with that
number of rows. }

\item{full.names}{logical; whether full names (as in
\code{\link{list.files}}) should be recorded. Must be \code{TRUE} if
\code{length(path) > 1}. }

\item{\dots}{ additional parameters to pass to
\code{\link{list.files}} to control the set of files in the snapshots.
}

\item{before, after}{objects produced by \code{fileSnapshot}; two
snapshots to compare.  If \code{after} is missing, a new snapshot of
the current file system will be produced for comparison, using
arguments recorded in \code{before} as defaults. }

\item{check.file.info}{character vector; which columns from
\code{\link{file.info}} should be compared. }

\item{x}{the object to print. }

\item{verbose}{logical; whether to list all data when printing. }

}
\details{

The \code{fileSnapshot} function uses \code{\link{list.files}} to
obtain a list of files, and depending on the \code{file.info},
\code{md5sum}, and \code{digest} arguments, records information about
each file.

The \code{changedFiles} function compares two snapshots. 

If the \code{timestamp} argument to \code{fileSnapshot} is length 1, a
file with that name is created. If it is length 1 in
\code{changedFiles}, the \code{\link{file_test}} function is used to
compare the age of all files common to both \code{before} and
\code{after} to it.  This test may be unreliable:  it compares the
current modification time of the \code{after} files to the timestamp;
that may not be the same as the modification time when the
\code{after} snapshot was taken.  It may also give incorrect results
if the clock on the file system holding the timestamp differs from the
one holding the snapshot files.

If the \code{check.file.info} argument contains a non-empty character
vector, the indicated columns from the result of a call to
\code{\link{file.info}} will be compared.

If \code{md5sum} is \code{TRUE}, \code{fileSnapshot} will call the
\code{tools::\link{md5sum}} function to record the 32 byte MD5
checksum for each file, and \code{changedFiles} will compare the
values.  The \code{digest} argument allows users to provide their own
digest function.

}

\value{

\code{fileSnapshot} returns an object of class \code{"fileSnapshot"}.
This is a list containing the fields 

\item{info}{a data frame whose rownames are the filenames, and whose
columns contain the requested snapshot data} 

\item{path}{the normalized \code{path} from the call} 

\item{timestamp, file.info, md5sum, digest, full.names}{a record of
the other arguments from the call} 

\item{args}{other arguments passed via \code{...} to
\code{\link{list.files}}.}

\code{changedFiles} produces an object of class \code{"changedFiles"}.
This is a list containing

\item{added, deleted, changed, unchanged}{character vectors of
filenames from the before and after snapshots, with obvious meanings}

\item{changes}{a logical matrix with a row for each common file, and a
column for each comparison test. \code{TRUE} indicates a change in
that test.}

\code{\link{print}} methods are defined for each of these types.  The
\code{\link{print}} method for \code{"fileSnapshot"} objects
displays the arguments used to produce them, while the one for
\code{"changedFiles"} displays the \code{added}, \code{deleted} and
\code{changed} fields if non-empty, and a submatrix of the
\code{changes} matrix containing all of the \code{TRUE} values.

}

\author{
Duncan Murdoch, using suggestions from Karl Millar and others.
}

\seealso{
\code{\link{file.info}}, \code{\link{file_test}}, \code{\link{md5sum}}.
}

\examples{
# Create some files in a temporary directory
dir <- tempfile()
dir.create(dir)
writeBin(1L, file.path(dir, "file1"))
writeBin(2L, file.path(dir, "file2"))
dir.create(file.path(dir, "dir"))

# Take a snapshot
snapshot <- fileSnapshot(dir, timestamp = tempfile("timestamp"), md5sum=TRUE)

# Change one of the files.
writeBin(3L:4L, file.path(dir, "file2"))

# Display the detected changes.  We may or may not see mtime change...
changedFiles(snapshot)
changedFiles(snapshot)$changes
}
\keyword{utilities}
\keyword{file}
