% File src/library/methods/man/findMethods.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2008-2019 R Core Team
% Distributed under GPL 2 or later

\name{findMethods}
\title{Description of the Methods Defined for a Generic Function}
\alias{findMethods}
\alias{findMethodSignatures}
\alias{hasMethods}
\alias{getMethods}
\alias{listOfMethods-class}
\description{
  The function \code{findMethods} converts the methods defined in a table for a generic
  function (as used for selection of methods) into a list, for study or
  display.  The list is actually from the class \code{listOfMethods}
  (see the section describing the class, below).


  The list will be limited
  to the methods defined in environment \code{where} if that argument is
  supplied and limited to those including one or more of the
  specified \code{classes} in the method signature if that argument is
  supplied.

  To see the actual table (an \code{\link{environment}}) used
  for methods dispatch, call \code{\link{getMethodsForDispatch}}.
  The names of the list returned by \code{findMethods} are the names of
  the objects in the table.

  The function \code{findMethodSignatures} returns a character matrix
  whose rows are the class names from the signature of the corresponding
  methods; it operates either from a list returned by
  \code{findMethods}, or by computing such a list itself, given the same
  arguments as \code{findMethods} .

  The function \code{hasMethods} returns \code{TRUE} or \code{FALSE}
  according to whether there is a non-empty table of methods for
  function \code{f} in the environment or search position \code{where}
  (or for the generic function generally if \code{where} is missing).

  The defunct function \code{getMethods} is an older alternative to
  \code{findMethods} , returning information in the form of an object of
  class \code{MethodsList}, previously used for method dispatch.  This
  class of objects is deprecated generally and will disappear in a
  future version of R.
}
\usage{
findMethods(f, where, classes = character(), inherited = FALSE,
      package = "")

findMethodSignatures(..., target = TRUE, methods = )

hasMethods(f, where, package)

## Deprecated in 2010 and defunct in 2015 for \code{table = FALSE}:
getMethods(f, where, table = FALSE)
}
\arguments{
  \item{f}{A generic function or the character-string name of one.}
  \item{where}{Optionally, an environment or position on the search list
    to look for methods metadata.

    If \code{where} is missing,  \code{findMethods} uses the current
    table of methods in the generic function itself, and
    \code{hasMethods} looks for metadata anywhere in the search list.
  }
  \item{table}{ If \code{TRUE} in a call to \code{getMethods} the
      returned value is the table used for dispatch, including
      inherited methods discovered to date.  Used internally, but
      since the default result is the now unused \code{mlist} object,
      the default will likely be changed at some point.
    }
  \item{classes}{If supplied, only methods whose signatures contain at
    least one of the supplied classes will be included in the value
    returned.}
  \item{inherited}{Logical flag; if \code{TRUE}, the table of all
    methods, inherited or defined directly, will be used; otherwise,
    only the methods explicitly defined.  Option \code{TRUE} is
    meaningful only if \code{where} is missing.}

  \item{...}{In the call to \code{findMethodSignatures}, any arguments
    that might be given to \code{findMethods}.}
  \item{target}{Optional flag to \code{findMethodSignatures}; if
    \code{TRUE}, the signatures used are the target signatures (the
    classes for which the method will be selected); if \code{FALSE},
    they will be the signatures are defined.  The difference is only
    meaningful if \code{inherited} is \code{TRUE}.}
  \item{methods}{In the call to \code{findMethodSignatures}, an optional
    list of methods, presumably returned by a previous call to
    \code{findMethods}.  If missing, that function will be call with the
    \dots arguments.}

  \item{package}{In a call to \code{hasMethods}, the package name for
    the generic function (e.g., \code{"base"} for primitives).  If
    missing this will be inferred either from the \code{"package"}
    attribute of the function name, if any, or from the package slot of
    the generic function.  See \sQuote{Details}.}
}
\details{

  The functions obtain a table of the defined methods, either from the
  generic function or from the stored metadata object in the environment
  specified by \code{where}.  In a call to \code{getMethods}, the information in the table is converted
  as described above to produce the returned value, except with the
  \code{table} argument.


  Note that \code{hasMethods}, but not the other functions, can be used
  even if no generic function of this name is currently found.  In this
  case \code{package} must either be supplied as an argument or included
  as an attribute of \code{f}, since the package name is part of the
  identification of the methods tables.

 }
\section{The Class for lists of methods}{
The class \code{"listOfMethods"} returns the methods as a named list
of method definitions (or a primitive function, see the slot
documentation below).  The names
are the strings used to store the corresponding objects in the
environment from which method dispatch is computed.
The current implementation uses the names of the corresponding classes
in the method signature, separated by \code{"#"} if more than one
argument is involved in the signature.
}
\section{Slots}{

  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"} The method
        definitions.

 Note that these may include the primitive function
  itself as  default method,
  when the generic corresponds to a primitive. (Basically, because
  primitive functions are abnormal R objects, which cannot currently be
  extended as method definitions.) Computations that use the returned
  list to derive other information need to take account of this
  possibility. See the implementation of \code{findMethodSignatures}
  for an example.
 }
    \item{\code{arguments}:}{Object of class \code{"character"}.  The
        names of the formal arguments in the signature of the generic function. }
    \item{\code{signatures}:}{Object of class \code{"list"}. A list of
      the signatures of the individual methods.  This is currently the
    result of splitting the \code{names} according to the \code{"#"}
    separator.

  If the object has been constructed from a table, as when returned by
\code{findMethods}, the signatures will all have the same length.
However, a list rather than a character matrix is used for
generality.  Calling \code{findMethodSignatures} as in the example
below will always convert to the matrix form.}
    \item{\code{generic}:}{Object of class \code{"genericFunction"}.
        The generic function corresponding to these methods.  There
        are plans to generalize this slot to allow reference to the function.}
    \item{\code{names}:}{Object of class \code{"character"}.  The
        names as noted are the class names separated by \code{"#"} .}
  }
}
\section{Extends}{

Class \code{"\linkS4class{namedList}"}, directly.

Class \code{"\linkS4class{list}"}, by class \code{"namedList"}, distance 2.

Class \code{"\linkS4class{vector}"}, by class \code{"namedList"}, distance 3.
}


\seealso{ \code{\link{showMethods}}, \code{\link{selectMethod}}, \link{Methods_Details} }
\examples{
mm <-  findMethods("Ops")
findMethodSignatures(methods = mm)
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
