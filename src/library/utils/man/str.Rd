\name{str}
\title{Compactly Display the Structure of an Arbitrary R Object}
\usage{
str(object, \dots)

\method{str}{data.frame}(object, \dots)

\method{str}{default}(object, max.level = 0, vec.len = 4, digits.d = 3,
    nchar.max = 128, give.attr = TRUE, give.length = TRUE,
    wid = getOption("width"), nest.lev = 0,
    indent.str = paste(rep(" ", max(0, nest.lev + 1)), collapse = ".."),
    \dots)

 ls.str(pos = 1, pattern, \dots,  envir = as.environment(pos),
        mode = "any")
lsf.str(pos = 1, \dots, envir = as.environment(pos))
\method{print}{ls_str}(x, max.level = 1, give.attr = FALSE, \dots)
}
\alias{str}
\alias{str.default}
\alias{str.data.frame}
\alias{ls.str}
\alias{lsf.str}
\alias{print.ls_str}
\description{
  Compactly display the internal \bold{str}ucture of an \R object, a
  \dQuote{diagnostic} function and an alternative to \code{\link{summary}}
  (and to some extent, \code{\link{dput}}).  Ideally, only one line for
  each \dQuote{basic} structure is displayed.  It is especially well suited
  to compactly display the (abbreviated) contents of (possibly nested)
  lists.  The idea is to give reasonable output for \bold{any} \R
  object.  It calls \code{\link{args}} for (non-primitive) function objects.

  \code{\link{ls.str}} and \code{\link{lsf.str}} are \dQuote{variations}
  of \code{\link{ls}} applying \code{str()} to each matched name, see
  section \sQuote{Value}.
}
\arguments{
  \item{object}{any \R object about which you want to have some
    information.}
  \item{max.level}{maximal level of nesting which is applied for
    displaying nested structures, e.g., a list containing sub lists.
    Default 0: Display all nesting levels.}
  \item{vec.len}{numeric (>= 0) indicating how many \dQuote{first few} elements
    are displayed of each vector.  The number is multiplied by different
    factors (from .5 to 3) depending on the kind of vector.  Default 4.}
  \item{digits.d}{number of digits for numerical components (as for
    \code{\link{print}}).}
  \item{nchar.max}{maximal number of characters to show for
    \code{\link{character}} strings.  Longer strings are truncated, see
    \code{longch} example below.}
  \item{give.attr}{logical; if \code{TRUE} (default), show attributes
    as sub structures.}
  \item{give.length}{logical; if \code{TRUE} (default), indicate
    length (as \code{[1:\dots]}).}
  \item{wid}{the page width to be used.  The default is the currently
    active \code{\link{options}("width")}.}
  \item{nest.lev}{current nesting level in the recursive calls to
    \code{str}.}
  \item{indent.str}{the indentation string to use.}
  \item{\dots}{potential further arguments (required for Method/Generic reasons).}
%% The following line is swallowed, unfortunately..  fixme??
  For \code{ls.str} and \code{lsf.str}:

  \item{pos}{integer indicating \code{\link{search}} path position.}
  \item{envir}{environment to use, see \code{\link{ls}}.}
  \item{pattern}{a \link{regular expression} passed to \code{\link{ls}}.
    Only names matching \code{pattern} are considered.}
  \item{mode}{character specifying the \code{\link{mode}} of objects to
    consider.  Passed to \code{\link{exists}} and \code{\link{get}}.}
%% print.ls_str:
  \item{x}{an object of class \code{"ls_str"}.}

}
\value{
  \code{str} does not return anything, for efficiency reasons.
  The obvious side effect is output to the terminal.

  \code{ls.str} and \code{lsf.str} return an object of class
  \code{"ls_str"}, basically the character vector of matching names
  (functions only for \code{\link{lsf.str}}), similarly to
  \code{\link{ls}}, with a \code{print()} method that calls \code{str()}
  on each object.
}
\author{Martin Maechler \email{maechler@stat.math.ethz.ch} since 1990.
}
\seealso{
  \code{\link{summary}}, \code{\link{args}}.
}
\examples{
require(stats)
## The following examples show some of 'str' capabilities
str(1:12)
str(ls)
str(args) #- more useful than  args(args) !
str(freeny)
str(str)
str(.Machine, digits = 20)
str( lsfit(1:9,1:9))
str( lsfit(1:9,1:9),  max = 1)
op <- options(); str(op) #- save first; otherwise internal options() is used.
need.dev <- !exists(".Device") || is.null(.Device)
if(need.dev) postscript()
str(par()); if(need.dev) graphics.off()

ch <- letters[1:12]; is.na(ch) <- 3:5
str(ch) # character NA's

nchar(longch <- paste(rep(letters,100), collapse=""))
str(longch)
str(longch, nchar.max = 52)

lsf.str()#- how do the functions look like which I am using?
ls.str(mode = "list") #- what are the structured objects I have defined?
## which base functions have "file" in their name ?
lsf.str(pos = length(search()), pattern = "file")
\dontshow{
 ##-- Some "crazy" objects
 str(array(1:5, dim=20))
 str(factor(character(0)))
 str(as.data.frame(NULL))
}
str(quote( { A+B; list(C,D) } ))

% package attaching/detaching only when needed:
\dontshow{
had.stats4 <- "package:stats4" \%in\% search()
if(!had.stats4)
   rs <-
}
## S4 classes :
require(stats4)
x <- 0:10; y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)
ll <- function(ymax=15, xh=6) -sum(dpois(y, lambda=ymax/(1+x/xh), log=TRUE))
fit <- mle(ll)
str(fit)
\dontshow{if(!had.stats4 && rs) detach("package:stats4")}
}
\keyword{print}
\keyword{documentation}
\keyword{utilities}
