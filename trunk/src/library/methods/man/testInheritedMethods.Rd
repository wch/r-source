% File src/library/methods/man/testInheritedMethods.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2009-2015 R Core Team
% Distributed under GPL 2 or later

\name{testInheritedMethods}
\alias{testInheritedMethods}
\alias{MethodSelectionReport-class}
\alias{.Other-class}
\title{
Test for and Report about  Selection of Inherited Methods
}
\description{
  A set of distinct inherited signatures is generated to test
  inheritance for all the methods of a specified generic function.  If
  method selection is ambiguous for some of these, a summary of the
  ambiguities is attached to the returned object.  This test should be
  performed by package authors \emph{before} releasing a package.
}
\usage{
testInheritedMethods(f, signatures, test = TRUE, virtual = FALSE,
                     groupMethods = TRUE, where = .GlobalEnv)
}
\arguments{
  \item{f}{a generic function or the character string name of one.  By default,
    all currently defined subclasses of all the method signatures for this
    generic will be examined.  The other arguments are mainly options to
    modify which inheritance patterns will be examined.
  }
  \item{signatures}{
    An optional set of subclass signatures to use instead of the relevant
    subclasses computed by \code{testInheritedMethods}.  See the Details
    for how this is done.  This argument might be supplied after a call
    with \code{test = FALSE}, to test selection in batches.
  }
  \item{test}{
    optional flag to control whether method selection is actually tested.
    If \code{FALSE}, returns just the list of relevant signatures for
    subclasses, without calling \code{\link{selectMethod}} for each signature.
    If there are a very large number of signatures, you may want to collect the full list  and then test them in batches.
  }
  \item{virtual}{
    should virtual classes be included in the relevant subclasses.
    Normally not, since only the classes of actual arguments will trigger
    the inheritance calculation in a call to the generic function.
    Including virtual classes may be useful if the class has no current
    non-virtual subclasses but you anticipate your users may define such
    classes in the future.
}
  \item{groupMethods}{
    should methods for the group generic function be included?
  }
  \item{where}{
    the environment in which to look for class definitions.  Nearly
    always, use the default global environment after attaching all the
    packages with relevant methods and/or class definitions.
  }
}
\details{
  The following description applies when the optional arguments are
  omitted, the usual case.
  First, the defining signatures for all methods are computed by calls
  to \code{\link{findMethodSignatures}}.
  From these all the known non-virtual subclasses are found for each
  class that appears in the signature of some method.
  These subclasses are split into groups according to which class they
  inherit from, and only one subclass from each group is retained (for
  each argument in the generic signature).
  So if a method was defined with class \code{"vector"} for some
  argument, one actual vector class is chosen arbitrarily.
  The case of \code{"ANY"} is dealt with specially, since all classes
  extend it.  A dummy, nonvirtual class, \code{".Other"}, is used to
  correspond to all classes that have no superclasses among those being
  tested.

  All combinations of retained subclasses for the
  arguments in the generic signature are then computed.
  Each row of the resulting matrix is a signature to be tested by a call
  to \code{\link{selectMethod}}.
  To collect information on ambiguous selections,
  \code{testInheritedMethods} establishes a calling handler for the
  special signal \code{"ambiguousMethodSelection"}, by setting the
  corresponding option.
}
\value{
  An object of class \code{"methodSelectionReport"}.  The details of
  this class are currently subject to change.  It has slots
  \code{"target"}, \code{"selected"}, \code{"candidates"}, and
  \code{"note"}, all referring to the ambiguous cases (and so of length
  0 if there were none).  These slots are intended to be examined by the
  programmer to detect and preferably fix ambiguous method selections.
  The object contains in addition slots \code{"generic"}, the name of
  the generic function, and
  \code{"allSelections"},  giving the vector of labels for all
  the signatures tested.
}
\references{
  Chambers, John M. (2008)
  \emph{Software for Data Analysis: Programming with R}
  Springer.  (Section 10.6 for basics of method selection.)

  Chambers, John M. (2009)
  \emph{Class Inheritance in R}
  \url{https://statweb.stanford.edu/~jmc4/classInheritance.pdf}.
}
\examples{
## if no other attached packages have methods for `+` or its group
## generic functions, this returns a 16 by 2 matrix of selection
## patterns (in R 2.9.0)
testInheritedMethods("+")
}

\keyword{programming}
\keyword{classes}
\keyword{methods}
