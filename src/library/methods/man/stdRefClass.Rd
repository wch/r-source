[% File src/library/methods/man/stdRefClass.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2010-2 R Core Team
% Distributed under GPL 2 or later

\name{envRefClass-class}
\Rdversion{1.1}
\docType{class}
\alias{envRefClass-class}
\alias{$,envRefClass-method}
\alias{$<-,envRefClass-method}
\alias{initialize,envRefClass-method}

\title{Class \code{"envRefClass"}}
\description{
Support Class to Implement R Objects using Reference Semantics
}
\section{NOTE:}{
The software described here is an initial version.  The eventual goal
is to support reference-style classes with software in \R itself
or using inter-system interfaces.  The current implementation (\R
version 2.12.0) is preliminary and subject to change, and currently
includes only the \R-only implementation.  Developers are encouraged
to experiment with the software, but the description here is more than
usually subject to change.
}
\section{Purpose of the Class}{
This class implements basic reference-style semantics for \R{}
objects.  Objects normally do not come directly from this class, but
from subclasses defined by a call to \code{\link{setRefClass}}.
The documentation below is technical background describing the implementation, but applications
should use the interface documented under \code{\link{setRefClass}},
in particular the \code{$} operator and field accessor functions as
described there.
}

\section{A Basic Reference Class}{
The design of reference classes for \R{} divides those classes up
according to the mechanism used for implementing references, fields,
and class methods.
Each version of this mechanism is defined by a \emph{basic reference
  class}, which must implement a set of methods and provide some
further information used by \code{\link{setRefClass}}.

The required methods are for operators \code{$} and \code{$<-} to
get and set a field in an object, and for \code{\link{initialize}} to
initialize objects.

To support these methods, the basic reference class needs to have some
implementation mechanism to store and retrieve data from fields in the
object.
The mechanism needs to be consistent with reference semantics; that
is, changes made to the contents of an object are global, seen by any
code accessing that object, rather than only local to the function
call where the change takes place.
As described below, class \code{envRefClass} implements reference
semantics through specialized use of \linkS4class{environment}
objects.
Other basic reference classes may use an interface to a language such
as Java or C++ using reference semantics for classes.

Usually, the \R  user will be able to invoke class methods on the
class, using the \code{$} operator.  The basic reference class
method for \code{$} needs to make this possible.  Essentially, the
operator must return an \R function corresponding to the object and
the class method name.

Class methods may include an implementation of data abstraction, in
the sense that fields are accessed by \dQuote{get} and \dQuote{set}
methods.  The basic reference class provides this facility by setting
the \code{"fieldAccessorGenerator"} slot in its definition to a
function of one variable.
This function will be called by \code{\link{setRefClass}} with the
vector of field names as arguments.
The generator function must return a list of defined accessor
functions.
An element corresponding to a get operation is invoked with no
arguments and should extract the corresponding field; an element for a
set operation will be invoked with a single argument, the value to be
assigned to the field.
The implementation needs to supply the object, since that is not an
argument in the method invocation.
The mechanism used currently by \code{envRefClass} is described below.
}
\section{Support Classes}{
Two virtual classes are supplied to test for reference objects:
\code{is(x, "refClass")} tests whether \code{x} comes from a class
defined using the reference class mechanism described here;
\code{is(x, "refObject")} tests whether the object has reference
semantics generally, including the previous classes and also classes
inheriting from the \R types with reference semantics, such as
\code{"environment"}.

Installed class methods are \code{"classMethodDefinition"} objects,
with slots that identify the name of the function as a class method
and the other class methods called from this method.
The latter information is determined heuristically when the class is
defined by using the \code{codetools} recommended package.  This
package must be installed when reference classes are defined, but is
not needed in order to use existing reference classes.
}

\author{
John Chambers
}
\keyword{classes}
