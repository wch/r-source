\name{Classes}
\alias{Classes}
\alias{__ClassMetaData}
\title{Class Definitions}
\description{
  Class definitions are objects that contain the formal definition of a
  class of R objects.
}
\details{
  When a class is defined, an object is stored that contains the
  information about that class, including:

  \describe{
    \item{slots}{Each slot is a component object.  Like elements of a
      list these may be extracted (by name) and set.  However, they
      differ from list components in important ways.

      All the objects from a particular class have the same set of slot
      names; specifically, the slot names that are contained in the
      class definition.  Each slot in each object always has the same
      class; again, this is defined by the overall class definition.

      Classes don't need to have any slots, and many useful classes do
      not.  These objects usually extend other, simple objects, such as
      numeric or character vectors.  Finally, classes can have no data
      at all---these are known as \emph{virtual} classes and are in fact
      very important programming tools.  They are used to group together
      ordinary classes that want to share some programming behavior,
      without necessarily restricting how the behavior is implemented.
    }
    \item{extends}{The names of the classes that this class extends.  A
      class \code{Fancy}, say, extends a class \code{Simple} if an
      object from the \code{Fancy} class has all the capabilities of
      the \code{Simple} class (and probably some more as well).  In
      particular, and very usefully, any method defined to work for a
      \code{Simple} object can be applied to a \code{Fancy} object as
      well.

      In other programming languages, this relationship is sometimes
      expressed by saying that \code{Simple} is a superclass of
      \code{Fancy}, or that \code{Fancy} is a subclass of
      \code{Simple}.

      The actual class definition object contains the names of all the
      classes this class extends.  But those classes can themselves
      extend other classes also, so the complete extension can only be
      known by obtaining all those class definitions.

      Class extension is usually defined when the class itself is
      defined, by including the names of superclasses as unnamed
      elements in the representation argument to \code{\link{setClass}}.

      An object from a given class will then have all the slots
      defined for its own class \emph{and} all the slots defined for
      its superclasses as well.

      Note that \code{extends} relations can be defined in other ways
      as well, by using the \code{\link{setIs}} function.
    }
    \item{prototype}{Each class definition contains a prototype object
      from the class.  This must have all the slots, if any, defined by
      the class definition.

      The prototype most commonly just consists of the prototypes of all
      its slots.  But that need not be the case:  the definition of the
      class can specify any valid object for any of the slots.

      There are a number of ``basic'' classes, corresponding to the
      ordinary kinds of data occurring in R.  For example,
      \code{"numeric"} is a class corresponding to numeric vectors.
      These classes are predefined and can then be used as slots or as
      superclasses for any other class definitions.  The prototypes for
      the vector classes are vectors of length 0 of the corresponding
      type.

      There are also a few basic virtual classes, the most important
      being \code{"vector"}, grouping together all the vector classes;
      and \code{"language"}, grouping together all the types of objects
      making up the R language.
    }
  }
}
\references{
  The web page \url{http://www.omegahat.org/RSMethods/index.html} is the
  primary documentation.

  The functions in this package emulate the facility for classes and
  methods described in \emph{Programming with Data} (John M. Chambers,
  Springer, 1998).  See this book for further details and examples.
}
\author{
  John Chambers
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
