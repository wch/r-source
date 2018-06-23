% File src/library/tools/man/package_native_routine_registration_skeleton.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2017 R Core Team
% Distributed under GPL 2 or later

\name{package_native_routine_registration_skeleton}
\alias{package_native_routine_registration_skeleton}
\title{
  Write Skeleton for Adding Native Routine Registration to a Package
}
\description{
  Write a skeleton for adding native routine registration to a package.
}
\usage{
package_native_routine_registration_skeleton(dir, con = stdout(),
    align = TRUE, character_only = TRUE, include_declarations = TRUE)
}
\arguments{
  \item{dir}{
    Top-level directory of a package.
  }
  \item{con}{
    Connection on which to write the skeleton: can be specified as a
    file path.
  }
  \item{align}{
    Logical: should the registration tables be lined up in three
    columns each?
  }
  \item{character_only}{
    Logical: should only \code{.NAME} arguments specified by character
    strings (and not as names of \R objects nor expressions) be extracted?
  }
  \item{include_declarations}{
    Logical: should the output include declarations (also known as
    \sQuote{prototypes}) for the registered routines?
  }
}
\details{
  Registration is described in section 5.4.1 of \sQuote{Writing R
    Extensions}. This function produces a skeleton of the C code which
  needs to be added to enable registration, conventionally as file
  \file{src/init.c} or appended to the sole C file of the package.

  This function examines the code in the \file{R} directory of the
  package for calls to \code{.C}, \code{.Fortran}, \code{.Call} and
  \code{.External} and creates registration information for those it can
  make sense of.  If the number of arguments used cannot be determined
  it will be recorded as \code{-1}: such values should be corrected.

  Optionally the skeleton will include declarations for the registered
  routines: they should be checked against the C/Fortran source code,
  not least as the number of arguments is taken from the \R code.  For
  \code{.Call} and \code{.External} calls they will often suffice, but
  for \code{.C} and \code{.Fortran} calls the \code{void *} arguments
  would ideally be replaced by the actual types.  Otherwise declarations
  need to be included (they may exist earlier in that file if appending
  to a file, or in a header file which can be included in
  \file{init.c}).
  
  The default value of \code{character_only} is appropriate when working
  on a package without any existing registration:
  \code{character_only = FALSE}
  can be used to suggest updates for a package which has been
  extended since registration.  For the default value, if \code{.NAME}
  values are found which are not character strings (e.g.\sspace{}names
  or expressions) this is noted via a comment in the output.

  Packages which used the earlier form of creating \R objects for native
  symbols \emph{via} additional arguments in a \code{useDynLib}
  directive will probably most easily be updated to use registration
  with \code{character_only = FALSE}.

  If an entry point is used with different numbers of arguments in the
  package's \R code, an entry in the table (and optionally, a
  declaration) is made for each number, and a comment placed in the
  output.  This needs to be resolved: only \code{.External} calls can
  have a variable number of arguments, which should be declared as
  \code{-1}.

  A surprising number of \acronym{CRAN} packages had calls in \R code to
  native routines not included in the package, which will lead to a
  \sQuote{loading failed} error during package installation when the
  registration C code is added.
  
  Calls which do not name a routine such as \code{.Call(\dots)} will be
  silently ignored.
}

\value{
  None: the output is written to the connection \code{con}.
}

\note{
  This only examines the \file{R} directory: it will not find
  e.g.\sspace\code{.Call} calls used directly in examples, tests \emph{etc}.
  
  Static code analysis is used to find the \code{.C} etc calls: it
  \emph{will} find those in parts of the \R code \sQuote{commented out}
  by inclusion in \code{if(FALSE) \{ \dots \}}.
  On the other hand, it will fail to find the entry points in
  constructs like \preformatted{
    .Call(if(int) "rle_i" else "rle_d", i, force)
  }
  and does not know the value of variables in calls like \preformatted{
    .Call (cfunction, ...)
    .Call(..., PACKAGE="sparseLTSEigen")
  } (but if \code{character_only} is false, will extract the first as
  \code{"cfunction"}).  Calls which have not been fully resolved will be
  noted \emph{via} comments in the output file.

  Call to entry points in other packages will be ignored if they have an
  explicit (character string) \code{PACKAGE} argument.
}

\section{Extracting C/C++ prototypes}{
  There are several tools available to extract function declarations
  from C or C++ code.  For example,

  For C code one can use \command{cproto}
  (\url{http://invisible-island.net/cproto/cproto.html}; Windows
  executables are available), for
  example\preformatted{
    cproto -I/path/to/R/include -e *.c
  }
  
  \command{ctags} (commonly distributed with the OS)
  covers C and C++., listing all function usages by something
  like\preformatted{
    ctags -x *.c
  }.  (The \sQuote{Exuberant} version allows a lot more control.)
}

\seealso{
  \code{\link{package.skeleton}}.
}
\examples{\dontrun{
## with a completed splines/DESCRIPTION file,
tools::package_native_routine_registration_skeleton('splines',,,FALSE)
## produces
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP spline_basis(SEXP, SEXP, SEXP, SEXP);
extern SEXP spline_value(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"spline_basis", (DL_FUNC) &spline_basis, 4},
    {"spline_value", (DL_FUNC) &spline_value, 5},
    {NULL, NULL, 0}
};

void R_init_splines(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
}}
