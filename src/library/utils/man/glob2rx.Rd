% File src/library/utils/man/glob2rx.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2024 R Core Team
% Copyright 2002-2024 The R Foundation
% Distributed under GPL 2 or later

\name{glob2rx}
\alias{glob2rx}
\title{Change Wildcard or Globbing Pattern into Regular Expression}
\description{
  Change \emph{wildcard} aka \emph{globbing} patterns into the
  corresponding regular expressions (\code{\link{regexp}}).

  This is also a practical didactical example for the use of
  \code{\link{sub}()} and regular expressions.
}
\usage{
glob2rx(pattern, trim.head = FALSE, trim.tail = TRUE)
}
\arguments{
  \item{pattern}{character vector}
  \item{trim.head}{logical specifying if leading \code{"^.*"} should be
    trimmed from the result.}
  \item{trim.tail}{logical specifying if trailing \code{".*$"} should be
    trimmed from the result.}
}
\details{
  This takes a wildcard as used by most shells and returns an equivalent
  regular expression.  \samp{?} is mapped to \samp{.} (match a single
  character), \samp{*} to \samp{.*} (match any string, including an
  empty one), and the pattern is anchored (it must start at the
  beginning and end at the end).  Optionally, the resulting regexp is
  simplified.

  Note that now even \samp{(}, \samp{[} and \samp{\{} can be used
  in \code{pattern}, but \code{glob2rx()} may not work correctly with
  arbitrary characters in \code{pattern}, for example escaped special
  characters.
}
\value{
  A character vector of the same length as the input \code{pattern}
  where each wildcard is translated to the corresponding
  regular expression.
}
\author{Martin Maechler, Unix/sed based version, 1991; current: 2004}
\seealso{
  \code{\link{regexp}} about regular expression,
  \code{\link{sub}}, etc about substitutions using regexps.
  \code{\link{Sys.glob}} does wildcard expansion, i.e., \dQuote{globbing} on
  file paths more subtly, e.g., allowing to escape special characters.
}
\examples{
stopifnot(glob2rx("abc.*") == "^abc\\\\.",% '\\' doubled in Rd file!
          glob2rx("a?b.*") == "^a.b\\\\.",
          glob2rx("a?b.*", trim.tail = FALSE) == "^a.b\\\\..*$",
          glob2rx("*.doc") == "^.*\\\\.doc$",
          glob2rx("*.doc", trim.head = TRUE) == "\\\\.doc$",
          glob2rx("*.t*")  == "^.*\\\\.t",
          glob2rx("*.t??") == "^.*\\\\.t..$",
          glob2rx("*[*")  == "^.*\\\\["
)
}
\keyword{file}
\keyword{character}
\keyword{utilities}

