% File src/library/utils/man/apropos.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{apropos}
\title{Find Objects by (Partial) Name}
\usage{
apropos(what, where = FALSE, ignore.case = TRUE, mode = "any")

find(what, mode = "any", numeric = FALSE, simple.words = TRUE)
}
\alias{apropos}
\alias{find}
\arguments{
  \item{what}{character string.  For \code{simple.words = FALSE}
    the name of an object; otherwise
    a \link{regular expression} to match object names against.}
  \item{where, numeric}{a logical indicating whether positions in the
    search list should also be returned}
  \item{ignore.case}{logical indicating if the search should be
    case-insensitive, \code{TRUE} by default.}
  \item{mode}{character; if not \code{"any"}, only objects whose
    \code{\link{mode}} equals \code{mode} are searched.}
  \item{simple.words}{logical; if \code{TRUE}, the \code{what} argument is
    only searched as a whole word.}
}
\description{
  \code{apropos()} returns a character vector giving the names of
  objects in the search list matching (as a regular expression)
  \code{what}.

  \code{find()} returns where objects of a given name can be found.
}
\details{
  If \code{mode != "any"} only those objects which are of mode \code{mode}
  are considered.

  \code{find} is a different user interface for a similar task to
  \code{apropos}. By default (\code{simple.words == TRUE}),
  only whole names are matched. Unlike \code{apropos}, matching is
  always case-sensitive.

  Unlike the default behaviour of \code{\link{ls}}, names which
  begin with a \samp{.} are included (and these are often
  \sQuote{internal} objects --- as from \R 3.4.0 most such are excluded).
}
\author{Originally, Kurt Hornik and Martin Maechler (May 1997).}
\value{
  For \code{apropos}, a character vector sorted by name.  For
  \code{where = TRUE} this has names giving the (numerical) positions on
  the search path.

  For \code{find}, either a character vector of environment names or
  (for \code{numeric = TRUE}) a numerical vector of positions on the
  search path with names the names of the corresponding environments.
}
\seealso{
  \code{\link{glob2rx}} to convert wildcard patterns to regular expressions.

  \code{\link{objects}} for listing objects from one place,
  \code{\link{help.search}} for searching the help system,
  \code{\link{search}} for the search path.
}
\examples{
require(stats)

%% some of these have enormous output that varies a lot by version
\dontrun{apropos("lm")}
apropos("GLM")                      # several
apropos("GLM", ignore.case = FALSE) # not one
apropos("lq")

cor <- 1:pi
find("cor")                         #> ".GlobalEnv"   "package:stats"
find("cor", numeric = TRUE)                     # numbers with these names
find("cor", numeric = TRUE, mode = "function")  # only the second one
rm(cor)

\dontrun{apropos(".", mode="list")  # a long list}

# need a DOUBLE backslash '\\\\' (in case you don't see it anymore)
apropos("\\\\[")

\donttest{# everything % not diff-able
length(apropos("."))

# those starting with 'pr'
apropos("^pr")

# the 1-letter things
apropos("^.$")
# the 1-2-letter things
apropos("^..?$")
# the 2-to-4 letter things
apropos("^.{2,4}$")

# the 8-and-more letter things
apropos("^.{8,}$")
table(nchar(apropos("^.{8,}$")))
}}
\keyword{data}
\keyword{documentation}
\keyword{environment}
