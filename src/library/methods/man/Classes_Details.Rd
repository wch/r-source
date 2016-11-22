% File src/library/methods/man/Classes.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{Classes_Details}
\alias{Classes_Details}
\alias{__ClassMetaData}
\alias{.environment-class}
\alias{.externalptr-class}
\alias{.name-class}
\alias{.NULL-class}
\title{Class Definitions}
\description{
  Class definitions are objects that contain the formal definition of a
  class of \R objects, usually referred to as an S4 class, to
  distinguish them from the informal S3 classes.
  This document gives an overview of S4 classes; for
  details of the class representation objects, see help for the class
  \code{\linkS4class{classRepresentation}}.
}
\section{Metadata Information}{
  When a class is defined, an object is stored that contains the
  information about that class.  The object, known as the
  \emph{metadata} defining the class, is not stored under the name of
  the class (to allow programmers to write generating functions of
  that name), but under a specially constructed name.
  To examine the class definition, call \code{\link{getClass}}.  The
  information in the metadata object includes:

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
      class definition.  Each slot in each object always is an object
      of  the
      class specified for this slot in the definition of the current class.
      The word \dQuote{is} corresponds to the \R function of the same
      name (\code{\link{is}}), meaning that the class of the object in
      the slot must be the same as the class specified in the
      definition, or some class that extends the one in the
      definition (a \emph{subclass}).

      A special slot name, \code{.Data}, stands for the
      \sQuote{data part} of the object.  An object from a class with a
      data part is defined by specifying that the class contains one
      of the \R object types or one of the special pseudo-classes,
      \code{matrix} or \code{array}, usually because the definition of
      the class, or of one of its superclasses, has included the type
      or pseudo-class in its \code{contains} argument.  A second
      special slot name, \code{.xData}, is used to enable inheritance
      from abnormal types such as \code{"environment"}
      See the section on inheriting from non-S4 classes
      for details on the representation and
      for the behavior of S3 methods with objects from these classes.

      Some slot names correspond to attributes used in old-style S3
      objects and in \R objects without an explicit class, for
      example, the \code{names} attribute.  If you define a class for
      which that attribute will be set, such as a subclass of named
      vectors, you should include \code{"names"} as a slot.  See the
      definition of class \code{"namedList"} for an example.  Using the
      \code{names()} assignment to set such names will generate a
      warning if there is no names slot and an error if the object in
      question is not a vector type.  A slot called \code{"names"} can
      be used anywhere, but only if it is assigned as a slot, not via
      the default \code{names()} assignment.

    }
    \item{Superclasses:}{

      The definition of a class includes the \emph{superclasses} ---the
      classes that this class extends.  A
      class \code{Fancy}, say, extends a class \code{Simple} if an
      object from the \code{Fancy} class has all the capabilities of
      the \code{Simple} class (and probably some more as well).  In
      particular, and very usefully, any method defined to work for a
      \code{Simple} object can be applied to a \code{Fancy} object as
      well.

      This relationship is expressed equivalently by saying that
      \code{Simple} is a superclass of \code{Fancy}, or that
      \code{Fancy} is a subclass of \code{Simple}.

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
      \code{\link{setClass}}, an object from the class will have all the
      slots defined for its own class \emph{and} all the slots defined
      for all its superclasses as well.

      The information about the relation between a class and a
      particular superclass is encoded as an object of class
      \code{\linkS4class{SClassExtension}}.  A list of such objects for
      the superclasses (and sometimes for the subclasses) is included in
      the metadata object defining the class.  If you need to compute
      with these objects (for example, to compare the distances), call
      the function \code{\link{extends}} with argument \code{fullInfo=TRUE}.

    }
    \item{Prototype:}{

      The objects from a class created by a call to
      \code{\link{new}}
      are defined by the \emph{prototype} object for the class and by
      additional arguments in the call to \code{\link{new}}, which are
      passed to a method for that class for the function
      \code{\link{initialize}}.

      Each class representation object contains a prototype object
      for the class (although for a virtual class the prototype may be
      \code{NULL}). The prototype object must have values for all the
      slots of the class.
      By default, these are the prototypes of the corresponding slot
      classes.  However, the definition of the class can specify any
      valid object for any of the slots.
    }
  }
}
\section{Basic classes}{

      There are a number of \sQuote{basic} classes, corresponding to the
      ordinary kinds of data occurring in \R.  For example,
      \code{"numeric"} is a class corresponding to numeric vectors.
      The other vector basic classes are \code{"logical"}, \code{"integer"},
      \code{"complex"}, \code{"character"},  \code{"raw"}, \code{"list"}
      and \code{"expression"}.
      The prototypes for
      the vector classes are vectors of length 0 of the corresponding
      type.  Notice that basic classes are unusual in that the
      prototype object is from the class itself.

      In addition to the vector classes there are also basic classes
      corresponding to objects in the
      language, such as \code{"function"} and \code{"call"}.
      These classes are subclasses of the virtual class \code{"language"}.
      Finally, there are object types and corresponding basic classes for
      \dQuote{abnormal} objects, such as \code{"environment"} and
      \code{"externalptr"}.
      These objects do not follow the
      functional behavior of the language; in particular, they are not
      copied and so cannot have attributes or slots defined locally.

     All these classes can be used as slots or as
      superclasses for any other class definitions, although they do
      not themselves come with an explicit class.  For the abnormal
      object types, a special mechanism is used to enable inheritance
      as described below.


}
% S3 classes

\section{Inheriting from non-S4 Classes}{
  A class definition can extend classes other than
  regular S4 classes, usually by specifying them in the
  \code{contains=} argument to \code{\link{setClass}}.  Three groups
  of such classes behave distinctly:
\enumerate{
\item
S3 classes, which must have been registered by a previous call to
\code{\link{setOldClass}} (you can check that this has been done
  by calling \code{\link{getClass}}, which should return a class that
  extends \linkS4class{oldClass});

\item
  One of the \R object types, typically a vector type, which then
    defines the type of the S4 objects, but also a type such as
    \code{\link{environment}} that can not be used directly as a type
    for an S4 object.  See
    below.

\item
  One of the pseudo-classes \code{\linkS4class{matrix}}
      and \code{\linkS4class{array}}, implying objects with
      arbitrary vector types plus the \code{dim} and \code{dimnames}
      attributes.
  }

  This section describes the approach to combining S4 computations
  with older S3 computations by using such classes as superclasses. The
  design goal is to allow the S4 class to inherit S3 methods and
  default computations in as consistent a form as possible.

  As part of a general effort to make the S4 and S3 code in R more
  consistent, when objects from an S4 class are used as the first
  argument to a non-default S3 method, either for an S3 generic function
  (one that calls \code{\link{UseMethod}}) or for one of the primitive
  functions that dispatches S3 methods, an effort is made to provide a
  valid object for that method.  In particular, if the S4 class extends
  an S3 class or \code{matrix} or \code{array}, and there is an S3
  method matching one of these classes, the S4 object will be coerced to
  a valid S3 object, to the extent that is possible given that there is
  no formal definition of an S3 class.

  For example, suppose \code{"myFrame"} is an S4 class that includes the
  S3 class \code{"data.frame"} in the \code{contains=} argument to
  \code{\link{setClass}}.  If an object from this S4 class is passed to
  a function, say \code{\link{as.matrix}}, that has an S3 method for
  \code{"data.frame"}, the internal code for \code{\link{UseMethod}}
  will convert the object to a data frame; in particular, to an S3
  object whose class attribute will be the vector corresponding to the
  S3 class (possibly containing multiple class names). Similarly for an
  S4 object inheriting from \code{"matrix"} or \code{"array"}, the S4
  object will be converted to a valid S3 matrix or array.

  Note that the conversion is \emph{not} applied when an S4 object is
  passed to the default S3 method.  Some S3 generics attempt to deal
  with general objects, including S4 objects.  Also, no transformation
  is applied to S4 objects that do not correspond to a selected S3
  method; in particular, to objects from a class that does not contain
  either an S3 class or one of the basic types.  See \code{\link{asS4}}
  for the transformation details.

      In addition to explicit S3 generic functions, S3 methods are
      defined for a variety of operators and functions implemented as
      primitives.  These methods are dispatched by some internal C
      code that operates partly through the same code as real S3
      generic functions and partly via special considerations (for
      example, both arguments to a binary operator are examined when
      looking for methods).  The same mechanism for adapting S4
      objects to S3 methods has been applied to these computations as
      well, with a few exceptions such as generating an error if an S4
      object that does not extend an appropriate S3 class or type is
      passed to a binary operator.

      The remainder of this section discusses the mechanisms for
      inheriting from  basic object types. See \code{\linkS4class{matrix}}
      or \code{\linkS4class{array}}
      for inhering from the matrix and array
      pseudo-classes, or from time-series.  For the
      corresponding details for inheritance
      from S3 classes, see \code{\link{setOldClass}}.

      An object from a class that directly and simply contains one
      of the basic object types in \R, has implicitly a corresponding
      \code{.Data} slot of that type, allowing computations to extract
      or replace the data part while leaving other slots
      unchanged. If the type is one that can accept attributes and is
      duplicated normally, the inheritance also determines the type of the
      object; if the class definition has a \code{.Data} slot
      corresponding to a normal type, the class of the
      slot determines the type of the object (that is, the value of
      \code{\link{typeof}(x)}).
      For such classes,  \code{.Data} is a pseudo-slot; that
      is, extracting or setting it modifies the non-slot data in the
      object.  The functions \code{\link{getDataPart}} and
      \code{\link{setDataPart}} are a cleaner, but essentially
      equivalent way to deal with the data part.

      Extending a basic type this way allows objects to
      use old-style code for the corresponding type as well as S4
      methods.  Any basic type can be used for \code{.Data}, but
      a few types are treated differently because they do not behave like ordinary objects;
      for example, \code{"NULL"}, environments, and external pointers.
      Classes extend these types by having a slot, \code{.xData},
      itself inherited from an internally defined S4 class.  This
      slot actually contains an object of the inherited type, to
      protect computations from the reference semantics of the type.
      Coercing to the nonstandard object type then requires an
      actual computation, rather than the \code{"simple"} inclusion
      for other types and classes.  The intent is that programmers
      will not need to take account of the mechanism, but one
      implication is that you should \emph{not} explicitly use the
      type of an S4 object to detect inheritance from an arbitrary
      object type.  Use
      \code{\link{is}} and similar functions instead.
}




\references{
 Chambers, John M. (2016)
 \emph{Extending R},
  Chapman & Hall.
(Chapters 9 and 10.)
}

\seealso{
  \code{\link{Methods_Details}} for analogous discussion of methods,
  \code{\link{setClass}} for details of specifying class definitions,
  \code{\link{is}},
  \code{\link{as}},
  \code{\link{new}},
  \code{\link{slot}}
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
