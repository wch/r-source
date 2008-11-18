% File src/library/methods/man/findMethods.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2008 R Core Development Team
% Distributed under GPL 2 or later

\name{findMethods}
\alias{findMethods}
\alias{findMethodSignatures}
\alias{hasMethods}
\alias{getMethods}
\title{Descriptions of the Methods Defined for a Generic Function}
\description{
These functions convert the methods defined in a table for a generic function (as used for selection of methods) into a list, for study or display, or provide other information summarizing the methods.

The function \code{findMethods} returns a list of the method definitions currently existing for generic function \code{f}, limited to the methods defined in environment \code{where} if that argument is supplied  and possibly limited to those including one or more of the specified \code{classes} in the method signature.

The function \code{findMethodSignatures} returns a character matrix whose rows are the class names from the signature of the corresponding methods; it operates either from a list returned by \code{findMethods}, or by computing such a list itself,  given the same arguments as \code{findMethods} .

The function \code{hasMethods} returns \code{TRUE} or \code{FALSE} according to whether there is a non-empty table of methods for function \code{f} in the environment or search position \code{where} (or anywhere on the search list if \code{where} is missing).

The function \code{getMethods} is an older alternative to \code{findMethods} , returning  information in the form of an object of class \code{MethodsList}, previously used for method dispatch.  It is not recommended, since the object returned is usually less convenient than the value of \code{findMethods}.
}
\usage{
findMethods(f, where, classes = character(), inherited = FALSE)
findMethodSignatures(..., target = TRUE, methods = )

hasMethods(f, where, package)

getMethods(f, where)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{A generic function or the character-string name of one.}
  \item{where}{Optionally, an environment or position on the search list to look for methods metadata.

If \code{where} is missing,  \code{findMethods} uses the current table of methods in the generic function itself, and  \code{hasMethods} looks for metadata anywhere in the search list.}
  \item{classes}{If supplied, only methods whose signatures contain at least one of the supplied classes will be included in the value returned. }
    \item{inherited}{Logical flag; if \code{TRUE}, the table of all methods, inherited or defined directly, will be used; otherwise, only the methods explicitly defined.  Option \code{TRUE} is meaningful only if \code{where} is missing.}

  \item{...}{In the call to \code{findMethodSignatures}, any arguments that might be given to \code{findMethods}.}
    \item{target}{Optional flag to \code{findMethodSignatures}; if \code{TRUE}, the signatures used are the target signatures (the classes for which the method will be selected); if \code{FALSE}, they will be the signatures are defined.  The difference is only meaningful if \code{inherited} is \code{TRUE}.}
    \item{methods}{In the call to \code{findMethodSignatures}, an optional list of methods, presumably returned by a previous call to \code{findMethods}.  If missing, that function will be call with the \dots arguments.}

      \item{package}{In a call to \code{hasMethods}, the package name for the generic function (e.g., \code{"base"} for primitives).  If missing this will be inferred either from the \code{"package"} attribute of the function name, if any, or from the package slot of the generic function.  See details below.}
}
\details{
The functions obtain a table of the defined methods, either from the generic function or from the stored metadata object in the environment specified by \code{where}.  The information in the table is converted as described above to produce the returned value.

Note that the list returned may contain the primitive function itself, when the generic corresponds to a primitive.  (Basically, because primitive functions are abnormal R objects, which cannot currently be extended as method definitions.)  Computations that use the returned list to derive other information need to take account of this possibility.  See the implementation of \code{findMethodSignatures} for an example.

Note that \code{hasMethods}, but not the other functions, can be used even if no generic function of this name is currently found.
In this case \code{package} must either be supplied as an argument or included as an attribute of \code{f}, since the package name is part of the identification of the methods tables.

These functions should be used in searching for or summarizing methods, rather than using information about how the metadata for methods is stored.
}
\seealso{ \code{\link{showMethods}}, \code{\link{MethodsList-class}} }
\examples{
mm <-  findMethods("Ops")
findMethodSignatures(methods = mm)
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
