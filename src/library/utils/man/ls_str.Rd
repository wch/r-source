% File src/library/utils/man/ls_str.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{ls.str}
\title{List Objects and their Structure}
\usage{
ls.str(pos = -1, name, envir, all.names = FALSE,
       pattern, mode = "any")

lsf.str(pos = -1, envir, \dots)

\method{print}{ls_str}(x, max.level = 1, give.attr = FALSE, \dots,
      digits = max(1, getOption("str")$digits.d))
}
\alias{ls.str}
\alias{lsf.str}
\alias{print.ls_str}
\description{
  \code{ls.str} and \code{lsf.str} are variations of \code{\link{ls}}
  applying \code{\link{str}()} to each matched name: see section Value.
}
\arguments{
  \item{pos}{integer indicating \code{\link{search}} path position, or
    \code{-1} for the current environment.}
  \item{name}{optional name indicating \code{\link{search}} path
    position, see \code{\link{ls}}.}
  \item{envir}{environment to use, see \code{\link{ls}}.}
  \item{all.names}{logical indicating if names which begin with a
    \code{.} are omitted; see \code{\link{ls}}.}
  \item{pattern}{a \link{regular expression} passed to \code{\link{ls}}.
    Only names matching \code{pattern} are considered.}
  \item{max.level}{maximal level of nesting which is applied for
    displaying nested structures, e.g., a list containing sub lists.
    Default 1: Display only the first nested level.}
  \item{give.attr}{logical; if \code{TRUE} (default), show attributes
    as sub structures.}
  \item{mode}{character specifying the \code{\link{mode}} of objects to
    consider.  Passed to \code{\link{exists}} and \code{\link{get}}.}
  \item{x}{an object of class \code{"ls_str"}.}
  \item{\dots}{further arguments to pass.  \code{lsf.str} passes them to
    \code{ls.str} which passes them on to \code{\link{ls}}.  The
    (non-exported) print method \code{print.ls_str} passes them to
    \code{\link{str}}.}
  \item{digits}{the number of significant digits to use for printing.}
}
\value{
  \code{ls.str} and \code{lsf.str} return an object of class
  \code{"ls_str"}, basically the character vector of matching names
  (functions only for \code{lsf.str}), similarly to
  \code{\link{ls}}, with a \code{print()} method that calls \code{\link{str}()}
  on each object.
}
\author{Martin Maechler}
\seealso{
  \code{\link{str}}, \code{\link{summary}}, \code{\link{args}}.
}
\examples{
require(stats)

lsf.str()  #- how do the functions look like which I am using?
ls.str(mode = "list")   #- what are the structured objects I have defined?

## create a few objects
example(glm, echo = FALSE)
ll <- as.list(LETTERS)
print(ls.str(), max.level = 0)# don't show details

## which base functions have "file" in their name ?
lsf.str(pos = length(search()), pattern = "file")

## demonstrating that  ls.str() works inside functions
## ["browser/debug mode"]:
tt <- function(x, y = 1) { aa <- 7; r <- x + y; ls.str() }
(nms <- sapply(strsplit(capture.output(tt(2))," *: *"), `[`, 1))
stopifnot(nms == c("aa", "r","x","y"))
}
\keyword{print}
\keyword{utilities}
