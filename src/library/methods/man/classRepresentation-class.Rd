% File src/library/methods/man/classRepresentation-class.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{classRepresentation-class}
\docType{class}
\alias{classRepresentation-class}
\title{Class Objects }
\description{  These are the objects that hold the definition of
  classes of objects.  They are constructed and stored as meta-data by
  calls to the function \code{\link{setClass}}.  Don't manipulate them
  directly, except perhaps to look at individual slots. }

\section{Slots}{
  \describe{
    \item{\code{slots}:}{A named list of the slots in this class; the
      elements of the list are the classes to which the slots must
      belong (or extend), and the names of the list gives the
      corresponding slot names.}
    \item{\code{contains}:}{A named list of the classes this class
      \sQuote{contains}; the elements of the list are objects of
      \code{\linkS4class{SClassExtension}}. The list may be only the
      direct extensions or all the currently known extensions (see the
      details).}
    \item{\code{virtual}:}{Logical flag, set to \code{TRUE} if this is
      a virtual class.}
    \item{\code{prototype}:}{The object that represents the standard
      prototype for this class; i.e., the data and slots returned by a
      call to \code{\link{new}} for this class with no special
      arguments.  Don't mess with the prototype object directly.}
    \item{\code{validity}:}{Optionally, a function to be used to test
      the validity of objects from this class.
      See \code{\link{validObject}}.}
    \item{\code{access}:}{Access control information.  Not currently used.}
    \item{\code{className}:}{The character string name of the class.}
    \item{\code{package}:}{The character string name of the package to
      which the class belongs.  Nearly always the package on which the
      metadata for the class is stored, but in operations such as
      constructing inheritance information, the internal package name
      rules.}
    \item{\code{subclasses}:}{A named list of the classes known to
      extend this class'; the elements of the list are objects of class
      \code{\linkS4class{SClassExtension}}.  The list is currently only
      filled in when completing the class definition (see the details).}
    \item{\code{versionKey}:}{Object of class \code{"externalptr"};
      eventually will perhaps hold some versioning information, but not
      currently used. }
    \item{\code{sealed}:}{Object of class \code{"logical"}; is this
      class sealed?  If so, no modifications are allowed. }
  }
}
\details{
  Class definitions are stored as metadata in various packages.
  Additional metadata supplies information on inheritance (the result of
  calls to \code{\link{setIs}}).  Inheritance information implied by the
  class definition itself (because the class contains one or more other
  classes) is also constructed automatically.

  When a class is to be used in an R session, this information is
  assembled to complete the class definition.  The completion is a
  second object of class \code{"classRepresentation"}, cached for the
  session or until something happens to change the information.  A call
  to \code{\link{getClass}} returns the completed definition of a class;
  a call to \code{\link{getClassDef}} returns the stored definition
  (uncompleted).

  In particular, completion fills in the upward- and downward-pointing
  inheritance information for the class, in slots \code{contains} and
  \code{subclasses} respectively.  It's in principle important to note
  that this information can depend on which packages are installed,
  since these may define additional subclasses or superclasses.
}
\seealso{
  See function \code{\link{setClass}} to supply the information in the
  class definition.
  See \link{Classes} for a more basic discussion of class information.
}
\keyword{classes}
