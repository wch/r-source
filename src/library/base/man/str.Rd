\name{str}
\title{Compactly Display the Structure of an Arbitrary R Object}
\usage{
str(object, ...)
str.data.frame(object, ...)
str.default(object, max.level = 0, vec.len = 4, digits.d = 3,
    give.attr = TRUE, give.length = TRUE,
    wid = .Options$width,
    nest.lev = 0,
    indent.str = paste(rep(" ", max(0, nest.lev + 1)), collapse = ".."))

ls.str(name, pattern, mode = "any", max.level = 1, give.attr = FALSE)
lsf.str(...)
}
\alias{str}
\alias{str.default}
\alias{str.data.frame}
\alias{ls.str}
\alias{lsf.str}
\arguments{
  \item{object}{any \R object about which you want to have some
    information.}
  \item{max.level}{maximal level of nesting which is applied for
    displaying nested structures, e.g., a list containing sub lists.
    Default 0: Display all nesting levels.}
  \item{vec.len}{numeric indicating how many ``first few'' elements are
    displayed of each vector. The number is multiplied by different
    factors (from .5 to 3) depending on the kind of vector.  Default 4.}
  \item{digits.d}{number of digits for numerical components (as for
    \code{\link{print}}).}
  \item{give.attr}{logical; if \code{TRUE} (default), show attributes
    as sub structures.}
  \item{give.length}{logical; if \code{TRUE} (default), indicate
    length (as \code{[1:\dots]}).}
  \item{wid}{the page width to be used.  The default is the currently
    active \code{\link{options}("width")}.}
  \item{nest.lev}{current nesting level in the recursive calls to
    \code{str}.}
  \item{indent.str}{the indentation string to use.}
}
\description{
  This is a ``diagnostic'' function, and an alternative to
  \code{\link{summary}} (and to some extent, \code{\link{dput}}).  Ideally,
  only one line for each ``basic'' structure is displayed.  It is especially
  well suited to compactly display the (abbreviated) contents of
  (possibly nested) lists.  The idea is to give reasonable output for
  \bold{any} \R object.  It calls \code{\link{args}} for (non-primitive)
  function objects.

  \code{\link{ls.str}} and \code{\link{lsf.str}} are useful ``versions''
  of \code{\link{ls}}, calling \code{str} on each object.  They are not
  foolproof and should rather not be used for programming, but are
  provided for their usefulness.
}
\value{
  Nothing, for efficiency reasons.
  The obvious side effect is output to the terminal.
}
\author{Martin Maechler \email{maechler@stat.math.ethz.ch} since 1990.
}
\seealso{
  \code{\link{summary}}, \code{\link{args}}.
}
\examples{
## The following examples show some of `str' capabilities
str(1:12)
str(ls)
str(args)#- more useful than  args(args) !
data(freeny); str(freeny)
str(str)
str(.Machine, digits = 20)
str( lsfit(1:9,1:9))
str( lsfit(1:9,1:9),  max =1)
op <- options(); str(op)#- save first; otherwise internal options() is used.
need.dev <- !exists(".Device") || is.null(.Device)
if(need.dev) postscript(); str(par()); if(need.dev) graphics.off()

lsf.str()#- how do the functions look like which I am using?
ls.str(mode = "list")#- what are the structured objects I have defined?
\testonly{
 ##-- Some "crazy" objects
 str(array(1:5, dim=20))
 str(factor(character(0)))
 str(as.data.frame(NULL))
}
}
\keyword{print}
\keyword{documentation}
\keyword{utilities}
