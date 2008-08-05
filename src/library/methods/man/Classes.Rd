% File src/library/methods/man/Classes.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{Classes}
\alias{Classes}
\alias{__ClassMetaData}
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

      One class name is special, \code{.Data}.  This stands for the
      \sQuote{data part} of the object.  Any class that contains one
      of the basic object types in \R, has implicitly a corresponding
      \code{.Data} slot of that type, allowing computations to extract
      or replace the data part while leaving other slots
      unchanged. The \code{.Data} slot also determines the type of the
      object; if \code{x} has a \code{.Data} slot, the type of the
      slot is the type of the object (that is, the value of
      \code{\link{typeof}(x)}). Extending a basic type this way allows objects to
      use old-style code for the corresponding type as well as S4
      methods.  Any basic type can be used for \code{.Data}, with the
      exception of a few that do not behave like ordinary objects;
      namely, \code{"NULL"}, environments, and external pointers.

      There is  one additional use of the data part, which is also an
      exception to the correspondence with the object's type.  The exception
      arises from the special treatment of \code{\linkS4class{matrix}}
      and \code{\linkS4class{array}} \dQuote{classes} in \R.
      Matrix and array objects are managed internally and recognized
      without regard to any class attribute; therefore, they can be
      used as the data part of a new class.  In this case, the object
      type for the new class depends on the type of the data in the
      matrix or array.

      If the new class does not have a data part as described above,
      the type of objects from the new class is
      \code{"S4"}.  

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
      \code{\linkS4class{SClassExtension}}.  A list of such objects for
      the superclasses (and sometimes for the subclasses) is included in
      the metadata object defining the class.  If you need to compute
      with these objects (for example, to compare the distances), call
      the function \code{\link{extends}} with argument \code{fullInfo=TRUE}.

    }
    \item{Prototype:}{

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
    }
  }
}
\section{Virtual classes; Basic classes}{

      Classes exist for which there are no actual objects, the
      \emph{virtual} classes, in fact a
      very important programming tool.  They are used to group together
      ordinary classes that want to share some programming behavior,
      without necessarily restricting how the behavior is implemented.
      Virtual class definitions may if you want include
      slots (to provide some common behavior without fully defining
      the object---see the class \code{\linkS4class{traceable}} for an example).

      A simple and useful form of virtual class is the \emph{class
        union}, a virtual class that is defined in a call to
      \code{\link{setClassUnion}} by listing one or
      more of subclasses (classes that extend the class union).  Class
      unions can include as subclasses basic data types (whose
      definition is otherwise sealed).

      There are a number of \sQuote{basic} classes, corresponding to the
      ordinary kinds of data occurring in R.  For example,
      \code{"numeric"} is a class corresponding to numeric vectors.
      The other vector basic classes are \code{"logical"}, \code{"integer"}, 
      \code{"complex"}, \code{"character"},  \code{"raw"}, \ code{"list"}
      and \ code{"expression"}.
      The prototypes for
      the vector classes are vectors of length 0 of the corresponding
      type.  Notice that basic classes are unusual in that the
      prototype object is from the class itself.

      In addition to the vector classes there are also basic classes corresponding to objects in the
      language, such as \code{"function"} and \code{"call"}.
      These classes are subclasses of the virtual class \code{"language"}.
      Finally, there are basic classes for
      specialized objects, such as \code{"environment"} and \code{"externalptr"}.
      The vector and language basic classes can be used as slots or as
      superclasses for any other class definitions. 
      The classes corresponding to other object types can be used as
      slots but not always as superclasses, since many of them do not follow the
      functional behavior of the language; in particular, they are not
      copied and so cannot have attributes or slots defined locally.

}
\section{S3 Classes}{

  Earlier, informal classes of objects (usually referred to as
  \sQuote{S3} classes) are used by many \R functions.  It is natural to
  consider including them as the class for a slot in a formal class,
  or even as a class to be extended by the new class.  This isn't
  prohibited but there are some disadvantages, and if you do want to
  include S3 classes, they should be declared by including them in a
  call to \code{\link{setOldClass}}.  Here are some considerations:

  \itemize{
  \item Using S3 classes somewhat defeats the purpose of defining a
    formal class:  An important advantage to your users is that a formal
    class provides guarantees of what the object contains (minimally,
    the classes of the slots and therfore what data they contain;
    optionally, any other requirements imposed by a validity method).

    But there is no guarantee whatever about the data in an object from
    an S3 class.  It's entirely up to the functions that create or
    modify such objects.  If you want to provide guarantees to your
    users, you will need a valdity method that explicitly checks the
    contents of S3-class objects.

  \item To get the minimal guarantee (that the object in a slot has, or
    extends, the class for the slot) you should ensure that the S3
    classes are known to \emph{be} S3 classes, possibly with S
    inheritance.  To do this, include a call to
    \code{\link{setOldClass}} for the S3 classes used.

    Otherwise, the S3 class is undefined (and the code used by
    \code{setClass} will issue a warning).  Slot assignments, for
    example, will not then check for possible errors.

  \item Current versions of \R (beginning with 2.8.0) try to
    accommodate S4 classes that extend S3 classes, that is, those
    including an S3 class in the \code{contains=} argument to
    \code{\link{setClass}} . Specifically, objects from  such classes
    will contain the S3 class as a slot, and some S3 computations will
    recognize the S3 class,
    including method dispatch and the function
    \code{\link{inherits}}.  See  \code{\link{S3Class}} for details.
    The S3 classes \emph{must} have been registered by a call to  \code{\link{setOldClass}}.
    The basic caution remains true however: There is no guarantee that all S3 computations will be
    compatible, and some are known not to be.

  \item These caveats apply to S3 classes; that is, objects with a
    class assigned by some \R function but without a formal class
    definition.  In contrast, the built-in vector types (\code{numeric},
    \code{list}, etc.) are generally fine as slots or for
    \code{contains=} classes.  These object
    types don't have formal slots, but the base code in the system
    essentially forces them to contain the type of data they claim to
    have.

    Objects with a \dQuote{class}  of \code{matrix} or \code{array} are somewhat in
    between.  They do not have an explicit S3 class, but do have one or
    two attributes.  There is no general problem in having these as
    slots, but because there is no guarantee of a dimnames slot, they
    don't work as formal classes.  The \code{ts} class, although also
    ancient in the S language, is implemented in \R essentially as an
    S3 class, with the implications noted above---not suitable for a
    \code{contains=} argument---but with a few S4
    methods defined.  See the documentation for class \code{\linkS4class{structure}} for more details.

  }
 }% S3 classes

\references{
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version.)

 Chambers, John M. (1998)
 \emph{Programming with Data}
 Springer (For the original S4 version.) 

  Chambers, John M.  and Hastie, Trevor  J. eds (1992)
  \emph{Statistical Models in S.}
  Wadsworth & Brooks/Cole (Appendix A for S3 classes.)

  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole. (Out of print.) (The description of
  vectors, matrix, array and time-series objects.)
}

\seealso{
  \code{\link{Methods}} for analogous discussion of methods,
  \code{\link{setClass}} for details of specifying class definitions,
  \code{\link{is}},
  \code{\link{as}},
  \code{\link{new}},
  \code{\link{slot}}
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
