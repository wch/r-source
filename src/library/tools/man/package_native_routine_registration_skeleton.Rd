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
    align = TRUE, include_declarations = FALSE)
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
  \item{include_declarations}{
    Logical: should the output include declarations for the registered
    routines?
  }
}
\details{
  Registration is described in section 5.4.1 of \sQuote{Writing R
    Extensions}. This function produces a skeleton of the C code which
  needs to be added to enable registration, conventionally as file
  \file{src/init.c} or appended to the sole C file of the package.

  If the number of arguments cannot be determined by the code it will
  be recorded as \code{-1}: such values should be corrected.

  Optionally the skeleton will include declarations for the registered
  routines: they should be checked against the C/Fortran source code,
  not least as the number of arguments is taken from the \R code.
  For \code{.Call} and \code{.External} calls they will often suffice,
  but for \code{.C} and \code{.Fortran} calls the \code{void *}
  arguments should be replaced by the actual types.  Otherwise
  declarations need to be included (they may exist earlier in the file
  if appending to a file, or in a header file which can be included in
  \file{init.c}).

  If an entry point is used with different numbers of arguments in the
  package's \R code, an entry in the table (and optionally, a
  declaration) is made for each number, and a comment placed in the
  output.  Obviously this needs to be resolved: only \code{.External}
  calls can have a variable number of arguments, which should be
  declared as \code{-1}.

  A surprisingly large number of \acronym{CRAN} packages had calls in \R
  code to native routines not included in the packages, which will lead
  to a compilation error when registration C code is added.
}

\value{
  None: the output is written to the connection \code{con}.
}

\note{
  This uses static code analysis to find the \code{.C} etc calls: it
  \emph{will} find those in parts of the \R code \sQuote{commented out} by
  inclusion in \code{if(FALSE) \{ \dots \}}.

  On the other hand, it will fail to find the entry points in
  constructs like \preformatted{
    .Call(if(int) "rle_i" else "rle_d", i, force)
  }
  and does not know the value of variables in calls like \preformatted{
    .Call (cfunction, ...)
    .Call(..., PACKAGE="sparseLTSEigen")
  }

  Call to entry points in other packages will be skipped if they have an
  explicit \code{PACKAGE} argument, but not if they use a symbol
  imported from another package.
}

\seealso{
  \code{\link{package.skeleton}}.
}
\examples{\dontrun{
## with a completed splines/DESCRIPTION file,
tools::package_native_routine_registration_skeleton('splines')
## produces
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* FIXME: 
   Add declarations for the native routines registered below.
*/

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
