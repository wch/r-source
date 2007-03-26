\name{Classes}
\alias{Classes}
\alias{__ClassMetaData}
\title{Class Definitions}
\description{
  Class definitions are objects that contain the formal definition of a
  class of R objects, usually referred to as an S4 class, to
  distinguish them from the informal S3 classes.
  This document gives an overview of S4 classes; for
  details of the  class representation objects themselves, see
  \code{class?classRepresentation} (\code{\link{classRepresentation-class}}).
}
\details{
  When a class is defined, an object is stored that contains the
  information about that class, including:

  \describe{
    \item{Slots:}{

        The data contained in an object from an S4 class is defined by
        the \emph{slots} in the class definition.

        Each slot in an object is a component of the object;
        like components (that is, elements) of a
      list, these may be extracted and set, using the
      function \code{\link{slot}()} or more often the operator
        \code{"\link{@}"}.  However, they
      differ from list components in important ways.
      First, slots can only be referred to by name, not by position,
      and there is no partial matching of names as with list elements.

      All the objects from a particular class have the same set of slot
      names; specifically, the slot names that are contained in the
      class definition.  Each slot in each object always has the same
      class; again, this is defined by the overall class definition.
      The phrase \dQuote{has the same class} means that the class of the object in
      the slot must be the same as the class specified in the
      definition, or some class that extends the one in the
      definition.

      One class name is special, \code{.Data}.  This stands for the
      \dQuote{data part} of the object.  Any class that contains one
      of the basic data types in \R{}, has implicitly a corresponding
      \code{.Data} slot of that type, allowing computations to extract
      or replace the data part while leaving other slots
      unchanged. The \code{.Data} slot also determines the type of the
      object; if \code{x} has a \code{.Data} slot, the type of the
      slot is the type of the object (that is, the value of
      \code{\link{typeof}(x)}.  Otherwise the type of the object is
      \code{"S4"}.  Extending a basic type this way allows objects to
      use old-style code for the corresponding type as well as S4
      methods.  Any basic type can be used for \code{.Data}, with the
      exception of a few that do not behave like ordinary objects;
      namely, \code{"NULL"}, environments, and external pointers.

      Classes exist for which there are no actual objects, the
      \emph{virtual} classes, in fact a
      very important programming tool.  They are used to group together
      ordinary classes that want to share some programming behavior,
      without necessarily restricting how the behavior is implemented.
      Virtual class definitions may if you want include
      slots (to provide some common behavior without fully defining
      the object---see \link{traceable-class} for an example).

      A simple and useful form of virtual class is the \emph{class
        union}, a virtual class that is defined in a call to
      \code{\link{setClassUnion}} by listing one or
      more of subclasses (classes that extend the class union).  Class
      unions can include as subclasses basic data types (whose
      definition is otherwise sealed).

    }
    \item{Superclasses:}{

      The definition of a class includes the \emph{superclasses} ---the classes that this class extends.  A
      class \code{Fancy}, say, extends a class \code{Simple} if an
      object from the \code{Fancy} class has all the capabilities of
      the \code{Simple} class (and probably some more as well).  In
      particular, and very usefully, any method defined to work for a
      \code{Simple} object can be applied to a \code{Fancy} object as
      well.

      This relationship is
      expressed equivalently by saying that \code{Simple} is a superclass of
      \code{Fancy}, or that \code{Fancy} is a subclass of
      \code{Simple}.

      The direct superclasses of a class are those superclasses
      explicitly defined.   Direct superclasses can be defined in
      three ways.  Most commonly, the superclasses are listed in the
      \code{contains=} argument in the call to \code{\link{setClass}}
      that creates the subclass.   In this case the subclass will
      contain all the slots of the superclass, and the relation
      between the class is called \emph{simple}, as it in fact is.
      Superclasses can also be defined
      explicitly by a call to \code{\link{setIs}}; in this case, the
      relation requires methods to be specified to go from subclass to
      superclass.   Thirdly, a class union is a superclass of all the
      members of the union.  In this case too the relation is simple,
      but notice that the relation is defined when the superclass is
      created, not when the subclass is created as with the
      \code{contains=} mechanism.

      The definition of a superclass will also potentially contain
      its own direct superclasses.  These are considered (and shown) as
      superclasses at distance 2 from the original class; their direct
      superclasses are at distance 3, and so on.  All these are
      legitimate superclasses for purposes such as method selection.

      When superclasses are defined  by including the names of
      superclasses in the \code{contains=} argument to
      \code{\link{setClass}},   an object from the class will have all the slots
      defined for its own class \emph{and} all the slots defined for all
      its superclasses as well.

      The information about the relation between a class and a
      particular superclass is encoded as an object of class
      \code{"SClassExtension"} (see \link{SClassExtension-class}).  A
      list of such objects for the superclasses (and sometimes for the
      subclasses) is included in the metadata object defining the
      class.  If you need to compute with these objects (for example,
      to compare the distances), call the function
      \code{\link{extends}} with argument \code{fullInfo=TRUE}.

    }
    \item{Objects:}{

      The objects from a class, typically created by a call to
      \code{\link{new}} or by assigning another object from the class,
      are defined by the \emph{prototype} object for the class and by
      additional arguments in the call to \code{\link{new}}, which are
      passed to a method for that class for the function
      \code{\link{initialize}}.

      Each class definition contains a prototype object
      for the class.  This must have values for all the slots defined by
      the class definition.
      By default, these are the prototypes of all
      the slot classes, if those are not virtual classes.  However, the
      definition of the class can specify any valid object for any of
      the slots.

      There are a number of \dQuote{basic} classes, corresponding to the
      ordinary kinds of data occurring in R.  For example,
      \code{"numeric"} is a class corresponding to numeric vectors.
      There are also basic classes corresponding to objects in the
      language, such as \code{"function"} and \code{"call"}, and for
      specialized objects, such as \code{"environment"}
      These classes are predefined and can then be used as slots or as
      superclasses for any other class definitions.  The prototypes for
      the vector classes are vectors of length 0 of the corresponding
      type.  Notice that basic classes are unusual in that the
      prototype object is from the class itself.

      There are also a few basic virtual classes, the most important
      being \code{"vector"}, grouping together all the vector classes;
      and \code{"language"}, grouping together all the types of objects
      making up the R language.
    }
  }
}
\references{
  The functions in this package emulate the facility for classes and
  methods described in \emph{Programming with Data} (John M. Chambers,
  Springer, 1998).  See this book for further details and examples.
}

\seealso{
  \code{\link{Methods}},
  \code{\link{setClass}},
  \code{\link{is}},
  \code{\link{as}},
  \code{\link{new}},
  \code{\link{slot}}
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
