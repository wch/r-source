% File src/library/methods/man/showMethods.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
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
            showEmpty, printTo = stdout(), fdef)
}
\arguments{
  \item{f}{one or more function names.  If omitted, all functions
    will be shown that match the other arguments.

    The argument can also be an expression that evaluates to a single
    generic function, in which
    case argument \code{fdef} is ignored.  Providing an expression for
    the function allows examination of hidden or anonymous functions;
    see the example for \code{isDiagonal()}.}
  \item{where}{Where to find the generic function, if not supplied as an
    argument. When \code{f} is missing, or length 0, this also
    determines which generic functions to examine.  If \code{where} is
    supplied, only the generic functions returned by
    \code{getGenerics(where)} are eligible for printing.  If
    \code{where} is also missing, all the cached generic functions are
    considered.}
  \item{classes}{If argument \code{classes} is supplied, it is a vector
    of class names that restricts the displayed results to those methods
    whose signatures include one or more of those classes.}
  \item{includeDefs}{If \code{includeDefs} is \code{TRUE}, include the
    definitions of the individual methods in the printout.}
  \item{inherited}{logical indicating if methods that have been found by
    inheritance, so far in the session, will be included and marked as
    inherited.  Note that an inherited method will not usually appear
    until it has been used in this session.  See
    \code{\link{selectMethod}} if you want to know what method would be
    dispatched for particular classes of arguments.}
  \item{showEmpty}{logical indicating whether methods with no defined
    methods matching the other criteria should be shown at all.  By
    default, \code{TRUE} if and only if argument \code{f} is not
    missing.}
  \item{printTo}{The connection on which the information will be
    shown; by default, on standard output.}
  \item{fdef}{Optionally, the generic function definition to use; if
    missing, one is found, looking in \code{where} if that is specified.
    See also comment in \sQuote{Details}.}
}
\details{
  The name and package of the generic are followed by the list of
  signatures for which methods are currently defined, according to the
  criteria determined by the various arguments.  Note that the package
  refers to the source of the generic function.  Individual methods
  for that generic can come from other packages as well.

  When more than one generic function is involved, either as specified or
  because \code{f} was missing, the functions are found and
  \code{showMethods} is recalled for each, including the generic as the
  argument \code{fdef}.  In complicated situations, this can avoid some
  anomalous results.
}
\value{
  If \code{printTo} is \code{FALSE}, the character vector that would
  have been printed is returned; otherwise the value is the connection
  or filename, via \code{\link{invisible}}.
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
# Function "plot":
# x = ANY, y = track
# x = track, y = missing
# x = track, y = ANY

require("Matrix")
showMethods("\%*\%")# many!
    methods(class = "Matrix")# nothing
showMethods(class = "Matrix")# everything
showMethods(Matrix:::isDiagonal) # a non-exported generic
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
