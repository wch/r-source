% File src/library/utils/man/apropos.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
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
  \item{what}{character string with name of an object, or more generally
    a \link{regular expression} to match against.}
  \item{where, numeric}{a logical indicating whether positions in the
    search list should also be returned}
  \item{ignore.case}{logical indicating if the search should be
    case-insensitive, \code{TRUE} by default.  Note that in \R versions
    prior to 2.5.0, the default was implicitly \code{ignore.case = FALSE}.}
  \item{mode}{character; if not \code{"any"}, only objects whose
    \code{\link{mode}} equals \code{mode} are searched.}
  \item{simple.words}{logical; if \code{TRUE}, the \code{what} argument is
    only searched as whole word.}
}
\description{
  \code{apropos()} returns a character vector giving the names of
  all objects in the search list matching \code{what}.

  \code{find()} is a different user interface to the same task.
}
\details{
  If \code{mode != "any"} only those objects which are of mode \code{mode}
  are considered.
  If \code{where} is \code{TRUE}, the positions in the search list are
  returned as the names attribute.

  \code{find} is a different user interface for the same task as
  \code{apropos}. However, by default (\code{simple.words == TRUE}),
  only full words are searched with \code{\link{grep}(fixed = TRUE)}.
}
\author{Kurt Hornik and Martin Maechler (May 1997).}
\value{
  For \code{apropos} character vector, sorted by name, possibly with
  names giving the (numerical) positions on the search path.

  For \code{find}, either a character vector of environment names, or for
  \code{numeric = TRUE}, a numerical vector of positions on the search path,
  with names giving the names of the corresponding environments.
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
apropos("GLM")                      # more than a dozen
## that may include internal objects starting '.__C__' if
## methods is attached
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
