\name{ls.str}
\title{List Objects and their Structure}
\usage{
ls.str(pos = 1, pattern, \dots,  envir = as.environment(pos),
       mode = "any")

lsf.str(pos = 1, \dots, envir = as.environment(pos))

\method{print}{ls_str}(x, max.level = 1, give.attr = FALSE, \dots)
}
\alias{ls.str}
\alias{lsf.str}
\alias{print.ls_str}
\description{
  \code{ls.str} and \code{lsf.str} are \dQuote{variations}
  of \code{\link{ls}} applying \code{\link{str}()} to each matched name, see
  section \sQuote{Value}.
}
\arguments{
  \item{pos}{integer indicating \code{\link{search}} path position.}
  \item{pattern}{a \link{regular expression} passed to \code{\link{ls}}.
    Only names matching \code{pattern} are considered.}
  \item{max.level}{maximal level of nesting which is applied for
    displaying nested structures, e.g., a list containing sub lists.
    Default 0: Display all nesting levels.}
  \item{give.attr}{logical; if \code{TRUE} (default), show attributes
    as sub structures.}
  \item{envir}{environment to use, see \code{\link{ls}}.}
  \item{mode}{character specifying the \code{\link{mode}} of objects to
    consider.  Passed to \code{\link{exists}} and \code{\link{get}}.}
  \item{x}{an object of class \code{"ls_str"}.}
  \item{\dots}{further arguments to pass.  and \code{lsf.str} passes
    them to \code{ls.str} which passes them on to \code{\link{ls}}.
    The (non-exported) print method \code{print.ls_str} passes them to
    \code{\link{str}}.}
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
lsf.str()#- how do the functions look like which I am using?
ls.str(mode = "list") #- what are the structured objects I have defined?

## create a few objects
example(glm, echo = FALSE)
ll <- as.list(LETTERS)
print(ls.str(), max.level = 0)# don't show details

## which base functions have "file" in their name ?
lsf.str(pos = length(search()), pattern = "file")

}
\keyword{print}
\keyword{utilities}
