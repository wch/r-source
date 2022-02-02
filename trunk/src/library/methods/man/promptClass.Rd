% File src/library/methods/man/promptClass.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{promptClass}
\alias{promptClass}
\title{Generate a Shell for Documentation of a Formal Class}
\description{
  Assembles all relevant slot and method information for a class, with
  minimal markup for Rd processing; no QC facilities at present.
}
\usage{
promptClass(clName, filename = NULL, type = "class",
            keywords = "classes", where = topenv(parent.frame()),
            generatorName = clName)
}
\arguments{
  \item{clName}{a character string naming the class to be documented.}
  \item{filename}{usually, a connection or a character string giving the
    name of the file to which the documentation shell should be written.
    The default corresponds to a file whose name is the topic name for
    the class documentation, followed by \code{".Rd"}.  Can also be
    \code{NA} (see below).}
  \item{type}{the documentation type to be declared in the output file.}
  \item{keywords}{the keywords to include in the shell of the
    documentation.  The keyword \code{"classes"} should be one of
    them.}
  \item{where}{where to look for the definition of the class and of
    methods that use it.
    %%FIXME: By default, anywhere in the current search list.
  }
  \item{generatorName}{the name for a generator function for this
    class; only required if a generator function was created
    \emph{and} saved under a name different from the class name.
  }
}
\details{
  The class definition is found on the search list.  Using that
  definition, information about classes extended and slots is
  determined.

  In addition, the currently available generics with methods for this
  class are found (using \code{\link{getGenerics}}).  Note that these
  methods need not be in the same environment as the class definition; in
  particular, this part of the output may depend on which packages are
  currently in the search list.

  As with other prompt-style functions, unless \code{filename} is
  \code{NA}, the documentation shell is written to a file, and a message
  about this is given.  The file will need editing to give information
  about the \emph{meaning} of the class.  The output of
  \code{promptClass} can only contain information from the metadata
  about the formal definition and how it is used.

  If \code{filename} is \code{NA}, a list-style representation of the
  documentation shell is created and returned.  Writing the shell to a
  file amounts to \code{cat(unlist(x), file = filename, sep = "\\n")},
  where \code{x} is the list-style representation.

  If a generator function is found assigned under the class name or
  the optional \code{generatorName}, skeleton documentation for that
  function is added to the file.
}
\value{
  If \code{filename} is \code{NA}, a list-style representation of the
  documentation shell.  Otherwise, the name of the file written to is
  returned invisibly.
}
\author{
  VJ Carey \email{stvjc@channing.harvard.edu} and John Chambers
}
\references{
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version.)

 Chambers, John M. (1998)
 \emph{Programming with Data}
 Springer (For the original S4 version.)
}
\seealso{
  \code{\link{prompt}} for documentation of functions,
  \code{\link{promptMethods}} for documentation of method definitions.

  For processing of the edited documentation, either use
  \code{R CMD \link{Rdconv}},
  or include the edited file in the \file{man} subdirectory of a
  package.
}
\examples{
\dontshow{
## from setClass
## A simple class with two slots
setClass("track",
         slots = c(x="numeric", y="numeric"))
## A class extending the previous, adding one more slot
setClass("trackCurve", contains = "track",
         slots = c(smooth = "numeric"))
## A class similar to "trackCurve", but with different structure
## allowing matrices for the "y" and "smooth" slots
setClass("trackMultiCurve",
         slots = c(x="numeric", y="matrix", smooth="matrix"),
         prototype = list(x=numeric(), y=matrix(0,0,0), smooth= matrix(0,0,0)))

setIs("trackMultiCurve", "trackCurve",
  test = function(obj) {ncol(slot(obj, "y")) == 1},
  coerce = function(obj) { new("trackCurve", x = slot(obj, "x"),
        y = as.numeric(slot(obj,"y")), smooth = as.numeric(slot(obj, "smooth")))})

## from setMethod
require(graphics)

setMethod("plot", "track",
 function(x, y, ...) plot(slot(x, "y"), y,  ...)
)
setMethod("plot", c("trackCurve", "missing"),
function(x, y, ...) {
  plot(as(x, "track"))
  if(length(slot(x, "smooth") > 0))
    lines(slot(x, "x"), slot(x, "smooth"))
  }
)

promptClass("trackMultiCurve", stdout())

promptClass("track", stdout())
}
\dontrun{> promptClass("track")
A shell of class documentation has been written to the
file "track-class.Rd".
}
\dontshow{removeMethods("plot")
}}

\keyword{programming}
\keyword{classes}
