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
  mechanism; \bold{Dispatch and Method Selection} provides more
  details on how class definitions determine which methods are used.
  For additional information specifically about class definitions, see \code{?\link{Classes}}.
}

\section{How Methods Work}{
  A generic function is a function that has associated with it a
  collection of other functions (the methods), all of which agree in
  formal arguments with the generic.  

  Each R package will include  methods metadata objects
  corresponding to each generic function  for which methods have been
  defined in that package.
  When the package is loaded into an R session, the methods for each
  generic function are \emph{cached}, that is, stored in the
  environment of the generic function along with the methods from
  previously loaded packages.  This merged table of methods is used to
  dispatch or select methods from the generic, using class inheritance
  and possibly group generic functions to find an applicable method.
  See the \bold{Dispatch} section below.
  The caching computations ensure that only one version of each
  generic function is visible globally; although different attached
  packages may contain a copy of the generic function, these are in
  fact identical.

  The methods for a generic are stored according to the
  corresponding \code{signature} for which the method was defined, in
  a call to \code{\link{setMethod}}.  The signature associates one
  class name with each of a subset of the formal arguments to the
  generic function.  Which formal arguments are available, and the
  order in which they appear, are determined by the \code{"signature"}
  slot of the generic function.  By default, the signature of the
  generic consists of all the formal arguments except \dots, in the
  order they appear in the function definition.

  Trailing arguments in the signature will be \emph{inactive}  if no
  method has yet been specified that included those arguments.
  Inactive arguments are not needed or used in labeling the cached
  methods.  (The distinction does not change which methods are
  dispatched, but ignoring inactive arguments does improve the
  efficiency of dispatch.  Thus, defining the generic signature to
  contain the most useful arguments first can help efficiency
  somewhat.)

  All arguments in the signature of the generic function will be evaluated when the
  function is called, rather than using the traditional lazy
  evaluation rules of S.  Therefore, it's important to \emph{exclude}
  from the signature any arguments that need to be dealt with
  symbolically (such as the first argument to function
  \code{\link{substitute}}).  Note that only actual arguments are
  evaluated, not default expressions.
  A missing argument enters into the method selection as class
  \code{"missing"} and non-missing arguments according to their actual
  class.

  As of version 2.4.0 of R, the cached methods are stored in an
  environment object.  The names used for assignment are a
  concatenation of the class names for the arguments in the active
  signature. 

}
\section{Dispatch and Method Selection}{

When a call to a generic function is evaluated, a method is selected corresponding
to the classes of the actual arguments in the signature.
First, the cached methods table is searched for a \emph{direct} match;
that is, a method stored under the direct class names.
The direct class is the value of \code{class(x)} for each non-missing
argument, and class \code{"missing"} for each missing argument.
If no method is found directly for the actual arguments in a call to a
generic function, an attempt is made to match the available methods to
the arguments by using \emph{inheritance}.

Each class definition potentially includes the names of one or more
classes that the new class contains.  (These are sometimes called the
\emph{superclasses} of the new class.)  
The S language has an additional, explicit mechanism for defining superclasses, the
\code{\link{setIs}} mechanism.
Also, a call to \code{\link{setClassUnion}} makes the union class a
superclass of each of the members of the union.
All three mechanisms are treated equivalently for purposes of
inheritance:  they define the \emph{direct} superclasses of a
particular class.

The direct superclasses themselves may
contain other classes.  Putting all this information together produces
the full list of superclasses for this class. 
The superclass list is included in the definition of the class that is
cached during the R session.
Each element of the list describes the nature of the relationship (see
\link{SClassExtension-class} for details).
Included in the element is a \code{distance} slot giving a numeric
distance between the two classes.
The distance currently is the path length for the relationship:
\code{1} for direct superclasses (regardless of which mechanism
defined them), then \code{2} for the direct superclasses of those
classes, and so on.
In addition, any class implicitly has class \code{"ANY"} as a superclass.  The
distance to \code{"ANY"} is treated as larger than the distance to any
actual class.
The special class \code{"missing"} corresponding to missing arguments
has only \code{"ANY"} as a superclass, while \code{"ANY"} has no
superclasses.

The information about superclasses is summarized when a class
definition is printed.

When a method is to be selected by inheritance, a search is made in
the table for all methods directly corresponding to a combination of
either the direct class or one of its superclasses, for each argument
in the active signature.
For an example, suppose there is only one argument in the signature and that the class of
the corresponding object was \code{"dgeMatrix"} (from the
\code{Matrix} package on CRAN).
This class has two direct superclasses and through these 4 additional superclasses.
Method selection finds all the methods in the table of directly
specified methods labeled by one of these classes, or by
\code{"ANY"}.

When there are multiple arguments in the signature, each argument will
generate a similar  list of inherited classes.
The possible matches are now all the combinations of classes from each
argument (think of the function \code{outer} generating an array of
all possible combinations).
The search now finds all the methods matching any of this combination
of classes.
The computation of distances also has to combine distances for the
individual arguments.
There are many ways to combine the distances; the current
implementation simply adds them.
The result of the search is then a list of zero, one or more methods,
and a parallel vector of distances between the target signature and
the available methods.

If the list has more than one matching method,  only those corresponding to
the minimum distance are considered.
There may still be multiple best methods.
The dispatch software considers this an ambiguous case and warns the
user (only on the first call for this selection).
The method occurring first in the list of superclasses is selected.  By the mechanism of producing
the extension information, this orders the direct superclasses by the
order they appeared in the original call to \code{\link{setClass}},
followed by classes specified in \code{\link{setIs}} calls, in the
order those calls were evaluated, followed by classes specified in
unions.
Then the superclasses of those classes are appended (note that only
the ordering of classes within a particular generation of superclasses
counts, because only these will have the same distance).
For further discussion of method selection, see the document  \url{http://developer.r-project.org/howMethodsWork.pdf}.

All this detail about selection is less important than the realization
that having ambiguous method selection usually means that you need to
be more specific about intentions.
It is likely that some consideration other than the ordering of
superclasses in the class definition is more important in determining
which method \emph{should}  be selected, and the preference may well
be different for different generic functions.  Where ambiguities
arise, the best approach is usually to provide a specific method for
the subclass.

When the inherited method has been selected, the selection is cached
in the generic function so that future calls with the same class will
not require repeating the search.  Cached non-direct selections are
not themselves used in inheritance searches, since that could result
in invalid selections.

Besides being initiated through calls to the generic function, method
selection can be done explicitly by calling the function \code{\link{selectMethod}}.

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
  interface developed more recently than the reference.
}
\seealso{
  \code{\link{setGeneric}},
  \code{\link{setClass}}
 and the document  \url{http://developer.r-project.org/howMethodsWork.pdf}.
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
