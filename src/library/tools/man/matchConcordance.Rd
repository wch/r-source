% File src/library/tools/man/matchConcordance.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2022 Duncan Murdoch
% Distributed under GPL 2 or later

\name{matchConcordance}
\alias{matchConcordance}
\alias{as.character.Rconcordance}
\alias{as.Rconcordance}
\alias{as.Rconcordance.default}
\alias{Rconcordance}
\alias{activeConcordance}
\alias{followConcordance}
\title{
Concordance between source and target lines
}
\description{
The Rd parser records locations in \file{.Rd} files from
which components of the file are read.  Output generators
\code{\link{Rd2HTML}} and \code{\link{Rd2latex}} can output
information about these locations as \dQuote{concordances} between
source and output lines.  

\code{matchConcordance} converts from output locations
to source locations.  The \code{"Rconcordance"} method of
\code{as.character} produces
strings to embed in output files, and the default method of 
\code{as.Rconcordance}
converts these back to objects that can be interpreted
by \code{matchConcordance}.
}
\usage{
matchConcordance(linenum, concordance)
\method{as.character}{Rconcordance}(x, targetfile = "", ...)
as.Rconcordance(x, ...)
followConcordance(concordance, prevConcordance)
}
\arguments{
  \item{linenum}{
One or more line numbers being queried.
}
  \item{concordance}{
The concordance data for the file containing the lines: an object
of class \code{"Rconcordance"}.
}
  \item{prevConcordance}{
A concordance object retrieved from the current file.
}
  \item{targetfile}{
The output filename.  
  }
  \item{x}{
The object to convert:  for \code{as.character}, an \code{"Rconcordance"} object; for \code{as.concordance},
a character vector which contains \code{as.character} output, typically in comments.
  }
  \item{...}{
Further arguments passed to other methods.
  }
}
\details{
The correspondence between target lines and source lines 
in Rd file conversion
is not one to one.  Often a single source line can 
lead to the generation of multiple output lines, and 
sometimes more than one source line triggers output
on the same output line.

\code{matchConcordance} converts from target lines to
source lines.  This can be used to help in understanding how 
particular output lines
depend on the source, e.g. when an error is found in the 
output file.  When more than one line contributes
to the output, the last one will be returned.

The \code{"Rconcordance"} method of \code{as.character} converts a
concordance object
to strings suitable for embedding (e.g. in comments) in an
output file.

The default method of \code{as.Rconcordance} searches for strings matching the
pattern of \code{as.character.Rconcordance} output, then converts
those lines back to a single concordance.

\code{followConcordance} is used when a file is transformed
more than once.  The first transformation records a
concordance in the file which is read as \code{prevConcordance}. 
\code{followConcordance} chains this with the current 
\code{concordance}, relating the final result to the original
source.

There are 3 kinds of objects used to hold concordances.  

Objects of class \code{"activeConcordance"} are internal to \pkg{tools}; they
are used by \code{Rd2HTML} and \code{Rd2latex} while building the output
file and saving links to the source file.

Objects of class \code{"Rconcordance"} are visible to users.
They are list objects with the following three fields:
\describe{
  \item{\code{offset}}{The number of lines of output before the
  first one corresponding to this concordance.}
  \item{\code{srcLine}}{For each line of output after the offset,
the corresponding input file line number.  There may be more lines
of output than the length of \code{srcLine}, in which case nothing
can be inferred about the source of those lines.}
  \item{\code{srcFile}}{A vector of filenames of length 1 or the
same length as \code{srcLine} giving the source file(s) for
each output line.}
}

Concordance strings are produced by the \code{"Rconcordance"} method of
\code{as.character}; 
they are simply character vectors encoding the concordance data.
The default method of the \code{as.concordance} generic function 
converts them to \code{"Rconcordance"} objects.
}
\value{
\code{matchConcordance} returns a character array with one row 
per input \code{linenum} entry and
two columns, \code{"srcFile"} and \code{"srcLine"}.

For the \code{"Rconcordance"} method of \code{as.character}, a character
vector used (e.g. in \code{\link{Sweave}}) to embed 
the concordance in a file.

For \code{as.concordance}, an \code{"Rconcordance"} object, or \code{NULL}
if no concordance strings are found.
}
\author{
Duncan Murdoch
}
\seealso{
\code{\link{Rd2HTML}}, \code{\link{Rd2latex}}
}
