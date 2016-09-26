% File src/library/methods/man/findClass.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{findClass}
\title{Find Class Definitions}
\alias{removeClass}
\alias{resetClass}
\alias{isClass}
\alias{getClasses}
\alias{findClass}
\alias{sealClass}
\description{
  Functions to find classes:  \code{isClass} tests for a class;
  \code{findClass} returns the name(s) of packages containing the
  class; \code{getClasses} returns the names of all the classes in an
  environment, typically a namespace.  To examine the definition of a class, use \code{\link{getClass}}.
}
\usage{
isClass(Class, formal=TRUE, where)

getClasses(where, inherits = missing(where))

findClass(Class, where, unique = "")

## The remaining functions are retained for compatibility
## but not generally recommended

removeClass(Class, where)

resetClass(Class, classDef, where)

sealClass(Class, where)

}
\arguments{
  \item{Class}{character string name for the class.  The functions will
    usually take a class definition instead of the string.  To restrict
    the class to those defined in a particular package, set the
    \code{\link{packageSlot}} of the character string.}
  \item{where}{the \code{\link{environment}} in which to search for
    the class definition.  Defaults to the top-level environment of the
    calling function.  When called from the command line, this has the
    effect of using all the package environments in the search list.

    To restrict the search to classes in a particular package, use \code{where =
      asNamespace(pkg)} with \code{pkg} the package name;  to restrict
    it to
    the \emph{exported} classes, use \code{where = "package:pkg"} after the
    package is attached to the search list.
  }
  \item{formal}{\code{\link{logical}} is a formal definition
    required? For S compatibility, and always treated as \code{TRUE}.}
  \item{unique}{if \code{findClass} expects a unique location for the
    class, \code{unique} is a character string explaining the purpose
    of the search (and is used in warning and error messages).  By
    default, multiple locations are possible and the function always
    returns a list.
  }
  \item{inherits}{in a call to \code{getClasses}, should the value
    returned include all parent environments of \code{where}, or that
    environment only?  Defaults to \code{TRUE} if \code{where} is
    omitted, and to \code{FALSE} otherwise.
  }
  \item{classDef}{ For \code{resetClass}, the optional class
    definition.
  }
}

\section{Functions}{

  \describe{
    \item{\code{isClass}:}{
      Is this the name of a formally defined class?
    }
    \item{\code{getClasses}:}{
      The names of all the classes formally defined on \code{where}.  If
      called with no argument, all the classes visible from the
      calling function (if called from the top-level, all the classes
      in any of the environments on the search list).  The
      \code{where} argument is used to search only in a particular package.
    }
    \item{\code{findClass}:}{
      The list of environments in
      which a class definition of \code{Class} is found.  If
      \code{where} is supplied, a list is still returned, either empty
      or containing the environment corresponding to \code{where}.
      By default when called from the \R session, the global
      environment and all the currently
      attached packages are searched.

      If \code{unique} is supplied as a character string,
      \code{findClass} will warn if there is more than one definition
      visible (using the string to identify the purpose of the call),
      and will generate an error if no definition can be found.

      \emph{The remaining functions are retained for
        back-compatibility and internal use, but not generally recommended.}
    }
    \item{\code{removeClass}:}{
      Remove the definition of this class.  This can't be used if the
      class is in another package, and would rarely be needed in
      source code defining classes in a package.
    }
    \item{\code{resetClass}:}{
      Reset the internal definition of a class.  Not legitimate for a
      class definition not in this package and rarely needed otherwise.
    }
    \item{\code{sealClass}:}{ Seal the current definition of the specified
      class, to prevent further changes, by setting the corresponding
      slot in the class definition.  This is rarely used, since
      classes in loaded packages are sealed by locking their namespace.
    }
  }
}

\references{
 Chambers, John M. (2016)
 \emph{Extending R},
  Chapman & Hall.
(Chapters 9 and 10.)

 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer. (Chapter 9 has some details not in the later reference.)
}

\seealso{
  \code{\link{getClass}},
  \code{\link{Classes_Details}},
  \code{\link{Methods_Details}},
  \code{\link{makeClassRepresentation}}
}
%% should add Examples section
\keyword{programming}
\keyword{classes}
\keyword{methods}
