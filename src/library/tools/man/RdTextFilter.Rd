% File src/library/tools/man/RdTextFilter.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009-11 R Core Team
% Distributed under GPL 2 or later

\name{RdTextFilter}
\alias{RdTextFilter}
\title{
Select text in an Rd file.
}
\description{
This function blanks out all non-text in an Rd file,
for spell checking or other uses.
}
\usage{
RdTextFilter(ifile, encoding = "unknown", keepSpacing = TRUE,
             drop = character(), keep = character(),
             macros = file.path(R.home("share"), "Rd", "macros", "system.Rd"))
}
\arguments{
\item{ifile}{
  An input file specified as a filename or connection, or an \code{"Rd"} object
  from \code{\link{parse_Rd}}.
}
\item{encoding}{
  An encoding name to pass to \code{\link{parse_Rd}}.
}
\item{keepSpacing}{
  Whether to try to leave the text in the same lines and columns
  as in the original file.
}
\item{drop}{
  Additional sections of the Rd to drop.
}
  \item{keep}{
  Sections of the Rd file to keep.
}
  \item{macros}{
  Macro definitions to assume when parsing.  See \code{\link{parse_Rd}}.
}
}
\details{
  This function parses the Rd file, then walks through it, element by
  element.  Items with tag \code{"TEXT"} are kept in the same position as
  they appeared in the original file, while other parts of the file are
  replaced with blanks, so a spell checker such as \code{\link{aspell}}
  can check only the text and report the position in the original file.
  (If \code{keepSpacing} is \code{FALSE}, blank filling will not occur,
  and text will not be output in its original location.)

  By default, the tags \verb{\S3method}, \verb{\S4method},
  \verb{\command}, \verb{\docType}, \verb{\email}, \verb{\encoding},
  \verb{\file}, \verb{\keyword}, \verb{\link}, \verb{\linkS4class},
  \verb{\method}, \verb{\pkg}, and \verb{\var} are skipped.  Additional
  tags can be skipped by listing them in the \code{drop} argument;
  listing tags in the \code{keep} argument will stop them from being
  skipped.  It is also possible to \code{keep} any of the
  \code{c("RCODE", "COMMENT", "VERB")} tags, which correspond to R-like
  code, comments, and verbatim text respectively, or to drop
  \code{"TEXT"}.
}
\value{
  A character vector which if written to a file, one element per line, would
  duplicate the text elements of the original Rd file.
}
\author{
  Duncan Murdoch
}
\note{
  The filter attempts to merge text elements into single words when markup in
  the Rd file is used to highlight just the start of a word.
}

\seealso{
  \code{\link{aspell}}, for which this is an acceptable \code{filter}.
}
\keyword{ utilities }
\keyword{ documentation }
