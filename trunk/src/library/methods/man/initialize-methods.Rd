\name{initialize-methods}
\docType{methods}
\title{Methods to Initialize New Objects from a Class}
\alias{initialize-methods}
\alias{initialize,ANY-method}
\alias{initialize,traceable-method}
\alias{initialize,signature-method}
\alias{initialize,environment-method}
\description{
  The arguments to function \code{\link{new}} to create an object from a
  particular class can be interpreted specially for that class, by the
  definition of a method for function \code{initialize} for the class.
  This documentation describes some existing methods, and also outlines
  how to write new ones.
}
\section{Methods}{
  \describe{
    \item{.Object = "ANY"}{
      The default method for \code{initialize} takes either named or
      unnamed arguments.  Argument names must be the names of slots in
      this class definition, and the corresponding arguments must be
      valid objects for the slot (that is, have the same class as
      specified for the slot, or some superclass of that class).  If the
      object comes from a superclass, it is not coerced strictly, so
      normally it will retain its current class (specifically,
      \code{\link{as}(object, Class, strict = FALSE)}).

      Unnamed arguments must be objects of this class, of one of its
      superclasses, or one of its subclasses (from the class, from a
      class this class extends, or from a class that extends this
      class). If the object is from a superclass, this normally defines
      some of the slots in the object.  If the object is from a
      subclass, the new object is that argument, coerced to the current
      class.

      Unnamed arguments are processed first, in the order they appear.
      Then named arguments are processed.  Therefore, explicit values
      for slots always override any values inferred from superclass or
      subclass arguments.
    }

    \item{.Object = "traceable"}{
      Objects of a class that extends \code{traceable} are used to
      implement debug tracing (see \link{traceable-class} and
      \code{\link{trace}}).

      The \code{initialize} method for these classes takes special
      arguments \code{def, tracer, exit, at, print}.  The first of these
      is the object to use as the original definition (e.g., a
      function).  The others correspond to the arguments to
      \code{\link{trace}}.
    }

    \item{.Object = "environment"}{
      The \code{initialize} method for environments takes a named list
      of objects to be used to initialize the environment.
    }

    \item{.Object = "signature"}{
      This is a method for internal use only.
      It takes an optional \code{functionDef} argument to provide a
      generic function with a \code{signature} slot to define the
      argument names.  See \link{Methods} for details.
    }
  }
}
\section{Writing Initialization Methods}{
  Initialization methods provide a general mechanism corresponding to
  generator functions in other languages.

  The arguments to \code{\link{initialize}} are \code{.Object} and
  \dots. Nearly always, \code{initialize} is called from \code{new},
  not directly.  The \code{.Object} argument is then the
  prototype object from the class.

  Two techniques are often appropriate for \code{initialize} methods:
  special argument names and \code{callNextMethod}.

  You may want argument names that are more natural to your users than
  the (default) slot names.  These will be the formal arguments to
  your method definition, in addition to \code{.Object} (always) and
  \dots (optionally).  For example, the method for class
  \code{"traceable"} documented above would be created by a call to
  \code{\link{setMethod}} of the form:

  \preformatted{
    setMethod("initialize", "traceable",
      function(.Object, def, tracer, exit, at, print) \dots
    )
  }

  In this example, no other arguments are meaningful, and the resulting
  method will throw an error if other names are supplied.

  When your new class extends another class, you may want to call the
  initialize method for this superclass (either a special method or the
  default).  For example, suppose you want to define a method for your
  class, with special argument \code{x}, but you also want users to be
  able to set slots specifically.  If you want \code{x} to override the
  slot information, the beginning of your method definition might look
  something like this:
  
  \preformatted{
    function(.Object, x, ...) \{
      Object <- callNextMethod(.Object, ...)
      if(!missing(x)) \{ # do something with x
  }

  You could also choose to have the inherited method override, by first
  interpreting \code{x}, and then calling the next method.

}
\keyword{methods}
\keyword{programming}
