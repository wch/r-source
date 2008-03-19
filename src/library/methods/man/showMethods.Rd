% File src/library/methods/man/showMethods.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{showMethods}
\alias{showMethods}
\title{Show all the methods for the specified function(s)}
\description{
  Show a summary of the methods for one or more generic functions,
  possibly restricted to those involving specified classes.
}
\usage{
showMethods(f = character(), where = topenv(parent.frame()),
            classes = NULL, includeDefs = FALSE,
            inherited = !includeDefs,
            showEmpty, printTo = stdout(), fdef =)
}
\arguments{
  \item{f}{one or more function names.  If omitted, all functions
    will be shown that match the other arguments. }
  \item{where}{Where to find the generic function, if not supplied as an argument.
 When \code{f} is missing, or length 0, this also
    determines which generic functions to examine.  If \code{where} is
    supplied, only the generic functions returned by
    \code{getGenerics(where)} are eligible for printing.  If
    \code{where} is also missing, all the cached generic functions are
    considered.}
  \item{classes}{ If argument \code{classes} is supplied, it is a vector
    of class names that restricts the displayed results to those methods
    whose signatures include one or more of those classes. }
  \item{includeDefs}{ If \code{includeDefs} is \code{TRUE}, include the
    definitions of the individual methods in the printout. }
  \item{inherited}{logical indicating if methods that have been found by
    inheritance, so far in the session, will be included and marked as
    inherited.  Note that an inherited method will not usually appear
    until it has been used in this session.
    See \code{\link{selectMethod}} if you want to know what method is
    dispatched for particular classes of arguments.}
  \item{showEmpty}{logical indicating whether methods with no defined
    methods matching the other criteria should be shown at all.  By
    default, \code{TRUE} if and only if argument \code{f} is not
    missing.}
  \item{printTo}{The connection on which the printed information will be
    written; by default, standard output.  If \code{printTo} is
    \code{FALSE}, the output will be collected as a character vector and
    returned as the value of the call to \code{showMethod}.  See
    \code{\link{show}}.
  }
  \item{fdef}{Optionally, the generic function definition to use; if missing, one is found, looking in \code{where} if that is specified.  See also comment on details below.}
}
\details{
  The name and package of the generic are followed by the list of
  signatures for which methods are currently defined, according to the
  criteria determined by the various arguments.  Note that the package
  refers to the source of the generic function.  Individual methods
  for that generic can come from other packages as well.

When more than one generic function is involved, either as specified or because \code{f} was missing, the functions are found and \code{showMethods} is recalled for each, including the generic as the argument \code{fdef}.  In complicated situations, this can avoid some anomalous results.
}
\value{
  If \code{printTo} is \code{FALSE}, the character vector that would
  have been printed is returned; otherwise the value is the connection
  or filename, via \code{\link{invisible}}.
}
\references{
  The R package \pkg{methods} implements, with a few exceptions, the
  programming interface for classes and methods in the book
  \emph{Programming with Data} (John M. Chambers, Springer, 1998), in
  particular sections 1.6, 2.7, 2.8, and chapters 7 and 8.

  While the programming interface for the \pkg{methods} package follows
  the reference, the R software is an original implementation, so
  details in the reference that reflect the S4 implementation may appear
  differently in R.  Also, there are extensions to the programming
  interface developed more recently than the reference.  See
  \code{\link{Methods}} and references from there.

}
\seealso{
  \code{\link{setMethod}}, and \code{\link{GenericFunctions}}
  for other tools involving methods;
  \code{\link{selectMethod}} will show you the method dispatched for a
  particular function and signature of classes for the arguments.
}
\examples{
require(graphics)
\dontshow{
 setClass("track",
          representation(x="numeric", y="numeric"))
 ## First, with only one object as argument:
 setMethod("plot", signature(x="track", y="missing"),
           function(x,  y, ...) plot(slot(x, "x"), slot(x, "y"), ...))
 ## Second, plot the data from the track on the y-axis against anything
 ## as the x data.
 setMethod("plot", signature(y = "track"),
           function(x, y, ...) plot(x, slot(y, "y"), ...))
 setMethod("plot", "track",
           function(x, y, ...) plot(slot(x, "y"), y,  ...))
}
## Assuming the methods for plot
## are set up as in the example of help(setMethod),
## print (without definitions) the methods that involve class "track":
showMethods("plot", classes = "track")
\dontrun{
Function "plot":
x = ANY, y = track
x = track, y = missing
x = track, y = ANY

require("Matrix")# a CRAN package with much S4 functionality
showMethods("\%*\%")# many!
    methods(class = "Matrix")# nothing
showMethods(class = "Matrix")# everything
}%end{dontrun}

%% FIXME: This should be an example of showing methods from a loaded,
%% but not attached namespace. Except it doesn't work.
%% Much simpler situation of attach()ing when needed:
not.there <- !any("package:stats4" == search())
if(not.there) library(stats4)
showMethods(classes = "mle")
if(not.there) detach("package:stats4")
}
\keyword{methods}
