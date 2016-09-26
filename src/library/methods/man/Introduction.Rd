\name{Introduction}
\alias{Introduction}

\title{Basic use of S4 Methods and Classes}

\description{
The majority of applications using methods and classes will be in \R
packages implementing new computations for an application, using new \emph{classes}
of objects that represent the data and results.
Computations will be implemented using \emph{methods} that implement
functional computations when one or more of the arguments is an object
from these classes.

Calls to the functions \code{\link{setClass}()} define the new classes;
calls to \code{\link{setMethod}} define the methods.
These, along with ordinary \R computations, are sufficient to get
started for most applications.

Classes are defined in terms of the data in them and what other
classes of data they inherit from.
Section \sQuote{Defining Classes} outlines the basic design of new classes.

Methods are \R functions, often implementing basic computations as
they apply to the new classes of objects.
Section \sQuote{Defining Methods} discusses basic requirements and
special tools for defining methods.

The classes discussed here are the original functional classes.
\R also supports formal classes and methods similar to those in other
languages such as Python, in which methods are part of class
definitions and invoked on an object.
These are more appropriate when computations expect references to
objects that are persistent, making changes to the object over time.
See \link{ReferenceClasses} and Chapter 9 of the reference for the
choice between these and S4 classes.
}

\section{Defining Classes}{
All objects in \R belong to a class; ordinary vectors and other basic
objects are built-in (\link{builtin-class}).
A new class is defined in terms of the named \emph{slots} that is has
and/or in terms of existing classes that it inherits from, or
\emph{contains} (discussed in \sQuote{Class Inheritance} below).
A call to \code{\link{setClass}()} names a new class and uses the corresponding arguments to
define it.

For example, suppose we want a class of objects to represent a
collection of positions, perhaps from GPS readings.
A natural way to think of these in \R would have vectors of numeric values for
latitude, longitude and altitude.
A class with three corresponding slots could be defined by:

\code{
Pos <- setClass("Pos", slots = c(latitude = "numeric",
            longitude = "numeric", altitude = "numeric"))
}

The value returned is a function, typically assigned as here with the
name of the class.  Calling this function returns an object from the
class; its arguments are named with the slot names.
If a function in the class had read the corresponding data, perhaps
from a CSV file or from a data base, it could return an object from
the class by:

\code{Pos(latitude = x, longitude = y, altitude = z)}

The slots are accessed by the
\code{\link{@}} operator; for example, if \code{g} is an object from
the class, \code{g@latitude}.

In addition to returning a generator function the call to
\code{\link{setClass}()} assigns a definition of the class in a
special metadata object in the package's namespace.
When the package is loaded into an \R session, the class definition is
added to a table of known classes.

To make the class and the generating function publicly available, the
package should include \code{POS} in \code{exportClasses()} and
\code{export()} directives in its \code{NAMESPACE} file:

\code{exportClasses(Pos); export(Pos)}
}

\section{Defining Methods}{
Defining methods for an \R function makes that function
\emph{generic}.
Instead of a call to the function always being carried out by the same
method, there will be several alternatives.
These are selected by matching the classes of the arguments in the call to a
table  in the generic function, indexed by classes for one or more formal arguments to the
function, known as the \emph{signatures} for the methods.


A method definition then specifies three things:  the name of the
function, the signature and the method definition itself.
The definition must be a function with the same formal arguments as
the generic.

For example, a method to make a plot of an object from class
\code{"Pos"} could be defined by:

\code{setMethod("plot", c("Pos", "missing"), function(x, y, ...) \{
  plotPos(x, y) \})}

This method will match a call to \code{\link{plot}()} if the first
argument is from class \code{"Pos"} or a subclass of that.
The second argument must be missing; only a missing argument matches
that class in the signature.
Any object will match class \code{"ANY"} in the corresponding position
of the signature.
}

\section{Class Inheritance}{

A class may inherit all the slots and methods of one or more existing
classes by specifying the names of the inherited classes in the \code{contains =} argument to
\code{\link{setClass}()}.

To define a class that extends the \code{"Pos"} to a class
\code{"GPS"} with a slot for the observation times:

\code{GPS <- setClass("GPS", slots = c(time = "POSIXt"), contains = "Pos")}

The inherited classes may be S4 classes, S3
classes or basic data types.
S3 classes need to be identified as such by a call to
\code{\link{setOldClass}()}; most S3 classes in the base package and
many in the other built-in packages are already declared, as is
\code{"POSIXt"}.
If it had not been, the application package should contain:

\code{setOldClass("POSIXt")}

Inheriting from one of the \R types is special.  Objects from the new
class will have the same type.  A class
\code{Currency} that contains numeric data plus a slot \code{"unit"}
would be created by

\code{Currency <- setClass("Currency", slots = c(unit = "character"),
  contains = "numeric")}

Objects created from this class will have type \code{"numeric"} and
inherit all the builtin arithmetic and other computations for that
type.
Classes can only inherit from at most one such type; if the class does
not inherit from a type, objects from the class will have type
\code{"S4"}.

}


\references{
 Chambers, John M. (2016)
 \emph{Extending R},
  Chapman & Hall.
(Chapters 9 and 10.)
}

