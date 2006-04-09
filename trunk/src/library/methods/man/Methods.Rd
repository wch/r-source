\name{Methods}
\alias{Methods}
\title{General Information on Methods}
\description{
  This documentation section covers some general topics on how methods
  work and how the \pkg{methods} package interacts with the rest of R.  The
  information is usually not needed to get started with methods and
  classes, but may be helpful for moderately ambitious projects, or when
  something doesn't work as expected.

  The section \bold{How Methods Work} describes the underlying
  mechanism; \bold{Class Inheritance and Method Selection} provides more
  details on how class definitions determine which methods are used.

  The section \bold{Changes with the Methods Package} outlines possible
  effects on other computations when running with package \pkg{methods}.
}
\section{How Methods Work}{
  A generic function is a function that has associated with it a
  collection of other functions (the methods), all of which agree in
  formal arguments with the generic.  In R, the \dQuote{collection} is an
  object of class \code{"\link{MethodsList}"}, which contains a named
  list of methods (the \code{methods} slot), and the name of one of the
  formal arguments to the function (the \code{argument} slot).  The
  names of the methods are the names of classes, and the corresponding
  element defines the method or methods to be used if the corresponding
  argument has that class.  For example, suppose a function \code{f} has
  formal arguments \code{x} and \code{y}.  The methods list object for
  that function has the object \code{as.name("x")} as its
  \code{argument} slot.  An element of the methods named \code{"track"}
  is selected if the actual argument corresponding to \code{x} is an
  object of class \code{"track"}.  If there is such an element, it can
  generally be either a function or another methods list object.

  In the first case, the function defines the method to use for any call
  in which \code{x} is of class \code{"track"}.  In the second case, the
  new methods list object defines the selection of methods depending on
  the remaining formal arguments, in this example, \code{y}.  The same
  selection process takes place, recursively, using the new methods list.
  Eventually, the selection returns either a function or \code{NULL},
  meaning that no method matched the actual arguments.

  Each method selected corresponds conceptually to a \emph{signature};
  that is a named list of classes, with names corresponding to some or
  all of the formal arguments.  In the previous example, if selecting
  class \code{"track"} for \code{x}, finding that the selection was
  another methods list and then selecting class \code{"numeric"} for
  \code{y} would produce a method associated with the signature
  \code{x = "track", y = "numeric"}.

  The actual selection is done recursively, but you can see the methods
  arranged by signature by calling the function
  \code{\link{showMethods}}, and objects with the methods arranged this
  way (in two different forms) are returned by the functions
  \code{\link{listFromMlist}} and \code{\link{linearizeMlist}}.

  In an R session, each generic function has a single methods list
  object defining all the currently available methods.  The session
  methods list object is created the first time the function is called
  by merging all the relevant method definitions currently visible.
  Whenever something happens that might change the definitions (such as
  attaching or detaching a package with methods for this function, or
  explicitly defining or removing methods), the merged methods list
  object is removed.  The next call to the function will recompute the
  merged definitions.

  When methods list are merged, they can come from two sources:
  \enumerate{
    \item Methods list objects for the same function anywhere on the
    current search list.  These are merged so that methods in an
    environment earlier in the search list override methods for the same
    function later in the search list.  A method overrides only 
    another method for the same signature.  See the comments on class
    \code{"ANY"} in the section on \bold{Inheritance}.
    \item Methods list objects corresponding the group generic
    functions, if any, for this function.  Any generic function can be
    defined to belong to a group generic.  The methods for the group
    generic are available as methods for this function.  The group
    generic can itself be defined as belong to a group; as a result
    there is a list of group generic functions.  A method defined for a
    function and a particular signature overrides a method for the same
    signature for that function's group generic.
  }
  Merging is done first on all methods for a particular function, and
  then over the generic and its group generics.

  The result is a single methods list object that contains all the
  methods \emph{directly} defined for this function.  As calls to the
  function occur, this information may be supplemented by
  \emph{inherited} methods, which we consider next.
}
\section{Class Inheritance and Method Selection}{
If no method is found directly for the actual arguments in a call to a
generic function, an attempt is made to match the available methods to
the arguments by using \emph{inheritance}.

Each class definition potentially includes the names of one or more
classes that the new class contains.  (These are sometimes called the
\emph{superclasses} of the new class.)  These classes themselves may
extend other classes.  Putting all this information together produces
the full list of superclasses for this class. (You can see this list
for any class \code{"A"} from the expression \code{extends("A")}.)
In addition, any class implicitly extends class \code{"ANY"}.
When all the superclasses are needed, as they are for dispatching
methods, they are ordered by how direct they are:  first, the direct
classes contained directly in the definition of this class, then the
superclasses of these classes, etc.

The S language has an additional, explicit mechanism for defining superclasses, the
\code{\link{setIs}} mechanism.
This mechanism allows a class to extend another even though they do
not have the same representation.  The extension is made possible by
defining explicit methods to \code{coerce} an object to its superclass
and to \code{replace} the data in the object corresponding to the
superclass.  The \code{\link{setIs}} mechanism will be used less often
and only when directly including the superclass does not make sense,
but once defined, the superclass acts just as directly contained
classes as far as method selection is concerned.

A method will be selected by inheritance if we can find a method in
the methods list for a signature corresponding to any
combination of superclasses for each of the relevant arguments.
The search for such a method is performed by the function
\code{\link{MethodsListSelect}}, working as follows.


  The generic, \code{f} say,  has a signature, which by default
  is all its formal arguments, except \dots (see
  \code{\link{setGeneric}}). For each of the formal arguments in that
  signature, in order, the class of the actual argument is matched
  against available methods.  A missing argument corresponds to class
  \code{"missing"}.  If no method corresponds to the class of the
  argument, the evaluator looks for a method corresponding to the
  the superclasses (the other classes that the actual class
  extends, always including
  \code{"ANY"}).  If no match is found, the dispatch fails, with an
  error.  (But if there is a default method, that will always match.)

  If the match succeeds, it can find either a single method, or a
  methods list.  In the first case, the search is over, and returns
  the method.  In the second case, the search proceeds, with the
  next argument in the signature of the generic.  \emph{That} search
  may succeed or fail.  If it fails, the dispatch will try again with
  the next best match for the current argument, if there is one.
  The last match always corresponds to class \code{"ANY"}.

The effect of this definition of the selection process is to order all
possible inherited methods, first by the superclasses for the first
argument, then within this by the superclasses for the second
argument, and so on.

}

\section{Changes with the Methods Package}{
  The \pkg{methods} package is designed to leave other computations in R
  unchanged.  There are, however, a few areas where the default
  functions and behavior are overridden when running with the methods
  package attached.  This section outlines those known to have some
  possible effect.

  \describe{

    \item{\code{class}:}{
      The \pkg{methods} package enforces the notion that every object
      has a class; in particular, \code{class(x)} is never \code{NULL},
      as it would be for basic vectors, for example, when not using
      \pkg{methods}. 

      In addition, when assigning a class, the value is required to be a
      single string.  (However, objects can have multiple class names if
      these were generated by old-style class computations.  The methods
      package does not hide the \dQuote{extra} class names.)

      Computations using the notion of \code{NULL} class attributes or
      of class attributes with multiple class names are not really
      compatible with the ideas in the \pkg{methods} package.  Formal
      classes and class inheritance are designed to give more flexible
      and reliable implementations of similar ideas.

      If you do have to mix the two approaches, any operations that use
      class attributes in the old sense should be written in terms of
      \code{attr(x, "class")}, not \code{class(x)}.  In particular, test
      for no class having been assigned with
      \code{is.null(attr(x, "class"))}.
    }

    \item{Printing:}{
      To provide appropriate printing automatically for objects with
      formal class definitions, the \pkg{methods} package overrides
      \code{print.default}, to look for methods for the generic function
      \code{show}, and to use a default method for objects with formal
      class definitions.

      The revised version of \code{print.default} is intended to produce
      identical printing to the original version for any object that
      does \emph{not} have a formally defined class, including honoring
      old-style print methods.  So far, no exceptions are known.
    }
  }
}
\references{
  The R package \pkg{methods} implements, with a few exceptions, the
  programming interface for classes
  and methods in the book \emph{Programming with Data} (John
  M. Chambers, Springer, 1998), in particular sections 1.6, 2.7, 2.8,
  and chapters 7 and 8.

  While the programming interface for the \pkg{methods} package follows
  the reference, the R software is an original implementation, so
  details in the reference that reflect the S4 implementation may appear
  differently in R.  Also, there are extensions to the programming
  interface developed more recently than the reference.  For a
  discussion of details and ongoing development, see the web page 
  \url{http://developer.r-project.org/methodsPackage.html} and the
  pointers from that page.
}
\seealso{
  \code{\link{setGeneric}},
  \code{\link{setClass}}
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
