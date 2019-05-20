% File src/library/base/man/Foreign.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{Foreign}
\alias{Foreign}
\alias{.C}
\alias{.Fortran}
\title{Foreign Function Interface}
\description{
  Functions to make calls to compiled code that has been loaded into \R.
}
\usage{
       .C(.NAME, \dots, NAOK = FALSE, DUP = TRUE, PACKAGE, ENCODING)
 .Fortran(.NAME, \dots, NAOK = FALSE, DUP = TRUE, PACKAGE, ENCODING)
}
\arguments{
  \item{.NAME}{a character string giving the name of a C function or
    Fortran subroutine, or an object of class
    \code{"\link{NativeSymbolInfo}"}, \code{"\link{RegisteredNativeSymbol}"}
    or \code{"\link{NativeSymbol}"} referring to such a name.}
  \item{\dots}{arguments to be passed to the foreign function.  Up to 65.}
  \item{NAOK}{if \code{TRUE} then any \code{\link{NA}} or
    \code{\link{NaN}} or \code{\link{Inf}} values in the arguments are
    passed on to the foreign function.  If \code{FALSE}, the presence of
    \code{NA} or \code{NaN} or \code{Inf} values is regarded as an error.}
  \item{PACKAGE}{if supplied, confine the search for a character string
    \code{.NAME} to the DLL given by this argument (plus the
    conventional extension, \file{.so}, \file{.dll}, \dots).

    This is intended to add safety for packages, which can ensure by
    using this argument that no other package can override their external
    symbols, and also speeds up the search (see \sQuote{Note}).}

  \item{DUP, ENCODING}{For back-compatibility, accepted but ignored.}
}

\details{
  These functions can be used to make calls to compiled C and Fortran 77
  code.  Later interfaces are \code{\link{.Call}} and
  \code{\link{.External}} which are more flexible and have better
  performance.

  These functions are \link{primitive}, and \code{.NAME} is always
  matched to the first argument supplied (which should not be named).
  The other named arguments follow \code{\dots} and so cannot be
  abbreviated.  For clarity, should avoid using names in the arguments
  passed to \code{\dots} that match or partially match \code{.NAME}.
}

\value{
  A list similar to the \code{\dots} list of arguments passed in
  (including any names given to the arguments), but reflecting any
  changes made by the C or Fortran code.
}

\section{Argument types}{
  The mapping of the types of \R arguments to C or Fortran arguments is

  \tabular{lll}{
    \R \tab     C \tab     Fortran\cr
    integer \tab int * \tab integer\cr
    numeric \tab double * \tab double precision\cr
    -- or -- \tab float * \tab real\cr
    complex \tab Rcomplex * \tab double complex\cr
    logical \tab int * \tab integer \cr
    character \tab char ** \tab [see below]\cr
    raw \tab unsigned char * \tab not allowed\cr
    list \tab SEXP *\tab not allowed\cr
    other \tab SEXP\tab not allowed\cr
  }

  \emph{Note:} The C types corresponding to \code{integer} and
  \code{logical} are \code{int}, not \code{long} as in S.  This
  difference matters on most 64-bit platforms, where \code{int} is
  32-bit and \code{long} is 64-bit (but not on 64-bit Windows).

  \emph{Note:} The Fortran type corresponding to \code{logical} is
  \code{integer}, not \code{logical}: the difference matters on some
  Fortran compilers.
  
  Numeric vectors in \R will be passed as type \code{double *} to C
  (and as \code{double precision} to Fortran) unless the argument has
  attribute \code{Csingle} set to \code{TRUE} (use
  \code{\link{as.single}} or \code{\link{single}}).  This mechanism is
  only intended to be used to facilitate the interfacing of existing C
  and Fortran code.

  The C type \code{Rcomplex} is defined in \file{Complex.h} as a
  \code{typedef struct {double r; double i;}}.  It may or may not be
  equivalent to the C99 \code{double complex} type, depending on the
  compiler used.

  Logical values are sent as \code{0} (\code{FALSE}), \code{1}
  (\code{TRUE}) or \code{INT_MIN = -2147483648} (\code{NA}, but only if
  \code{NAOK = TRUE}), and the compiled code should return one of these
  three values: however non-zero values other than \code{INT_MIN} are
  mapped to \code{TRUE}.

  Missing (\code{NA}) string values are passed to \code{.C} as the string
  "NA". As the C \code{char} type can represent all possible bit patterns
  there appears to be no way to distinguish missing strings from the
  string \code{"NA"}.  If this distinction is important use \code{\link{.Call}}.

  Using a character string with \code{.Fortran} is deprecated and will
  give a warning.  It passes the first (only) character string of a
  character vector as a C character array to Fortran: that may be usable
  as \code{character*255} if its true length is passed separately.  Only
  up to 255 characters of the string are passed back.  (How well this
  works, and even if it works at all, depends on the C and Fortran
  compilers and the platform.)

  Lists, functions are other \R objects can (for historical reasons) be
  passed to \code{.C}, but the \code{\link{.Call}} interface is much
  preferred.  All inputs apart from atomic vectors should be regarded as
  read-only, and all apart from vectors (including lists), functions and
  environments are now deprecated.
}

\section{Fortran symbol names}{
  All Fortran compilers known to be usable to compile \R map symbol names
  to lower case, and so does \code{.Fortran}.

  Symbol names containing underscores are not valid Fortran 77 (although
  they are valid in Fortran 9x).  Many Fortran 77 compilers will allow
  them but may translate them in a different way to names not containing
  underscores.  Such names will often work with \code{.Fortran} (since
  how they are translated is detected when \R is built and the
  information used by \code{.Fortran}), but portable code should not use
  Fortran names containing underscores.

  Use \code{.Fortran} with care for compiled Fortran 9x code: it may not
  work if the Fortran 9x compiler used differs from the Fortran 77
  compiler used when configuring \R, especially if the subroutine name
  is not lower-case or includes an underscore.  It is possible to use
  \code{.C} and do any necessary symbol-name translation yourself.
}

\section{Copying of arguments}{
  Character vectors are copied before calling the compiled code and to
  collect the results.  For other atomic vectors the argument is copied
  before calling the compiled code if it is otherwise used in the
  calling code.

  Non-atomic-vector objects are read-only to the C code and are never
  copied.

  This behaviour can be changed by setting
  \code{\link{options}(CBoundsCheck = TRUE)}.  In that case raw,
  logical, integer, double and complex vector arguments are copied both
  before and after calling the compiled code.  The first copy made is
  extended at each end by guard bytes, and on return it is checked that
  these are unaltered.  For \code{.C}, each element of a character
  vector uses guard bytes.
}

\note{
  If one of these functions is to be used frequently, do specify
  \code{PACKAGE} (to confine the search to a single DLL) or pass
  \code{.NAME} as one of the native symbol objects.  Searching for
  symbols can take a long time, especially when many namespaces are loaded.

  You may see \code{PACKAGE = "base"} for symbols linked into \R.  Do
  not use this in your own code: such symbols are not part of the API
  and may be changed without warning.
}

\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{dyn.load}}, \code{\link{.Call}}.

  The \sQuote{Writing R Extensions} manual.
}
\keyword{programming}
