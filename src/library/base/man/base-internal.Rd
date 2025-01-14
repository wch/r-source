% File src/library/base/man/base-internal.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{base-internal}
\alias{.subset}
\alias{.subset2}
\alias{.getRequiredPackages}
\alias{.getRequiredPackages2}
\alias{.formula2varlist}
% removed for R 3.6.
%\alias{testPlatformEquivalence}
\alias{.isMethodsDispatchOn}
\alias{sys.save.image}
\alias{sys.load.image}
\alias{.row_names_info}
\alias{.set_row_names}
\alias{.ArgsEnv}
\alias{.GenericArgsEnv}
\alias{findPackageEnv}
\alias{lazyLoadDBfetch}
\alias{.TAOCP1997init}
\alias{.gt}
\alias{.gtn}
%% moved from trace.Rd in 2.11.0
\alias{.primTrace}
\alias{.primUntrace}
\alias{.Date}
\alias{.POSIXct}
\alias{.POSIXlt}
\alias{.difftime}
\alias{.cache_class}
\alias{.popath}
\alias{.detach}
\alias{.maskedMsg}

\alias{.C_R_addTaskCallback}
\alias{.C_R_getTaskCallbackNames}
\alias{.C_R_removeTaskCallback}
\alias{.F_dchdc}
\alias{.F_dqrcf}
\alias{.F_dqrdc2}
\alias{.F_dqrqty}
\alias{.F_dqrqy}
\alias{.F_dqrrsd}
\alias{.F_dqrxb}
\alias{.F_dtrco}
% These are compiled in conditionally
\alias{.C_R_getbcprofcounts}
\alias{.C_R_startbcprof}
\alias{.C_R_stopbcprof}

%% Only on Windows:
\alias{.fixupGFortranStderr}
\alias{.fixupGFortranStdout}

\alias{.doWrap}
\alias{.doSortWrap}

\alias{.amatch_bounds}
\alias{.amatch_costs}
\alias{.check_tzones}
\alias{.encode_numeric_version}
\alias{.decode_numeric_version}
\alias{.make_numeric_version}
\alias{.expand_R_libs_env_var}
\alias{.kronecker}
\alias{.packageStartupMessage}
\alias{.rmpkg}
\alias{.set_ops_need_as_vector}

\title{Internal Objects in Package \pkg{base}}
\description{% 'most': see e.g., .detach
  Internal objects in the base package most of which are only user-visible
  because of the special nature of the base namespace.
}
\usage{
.subset(x, \dots)
.subset2(x, \dots)

.getRequiredPackages(file = "DESCRIPTION", lib.loc = NULL,
                     quietly = FALSE, useImports = FALSE)
.getRequiredPackages2(pkgInfo, quietly = FALSE, lib.loc = NULL,
                      useImports = FALSE)

%testPlatformEquivalence(built, run)
%
.isMethodsDispatchOn(onOff = NULL)

sys.load.image(name, quiet)
sys.save.image(name)

.row_names_info(x, type = 1L)
.set_row_names(n)

findPackageEnv(info)

lazyLoadDBfetch(key, file, compressed, hook)

.TAOCP1997init(seed)

.gt(x, i, j)
.gtn(x, strictly)

.primTrace(obj)
.primUntrace(obj)

.Date(xx, cl = "Date")
.POSIXct(xx, tz = NULL, cl = c("POSIXct", "POSIXt"))
.POSIXlt(xx, tz = NULL, cl = c("POSIXlt", "POSIXt"))
.difftime(xx, units, cl = "difftime")

.cache_class(class, extends)

.popath

.detach(pos)

.maskedMsg(same, pkg, by)

## Only on Windows:
.fixupGFortranStdout()
.fixupGFortranStderr()

.doWrap(vec, decr, nalast, noNA = NA) 
.doSortWrap(vec, decr, nalast, noNA = NA)

.amatch_bounds(x = 0.1) 
.amatch_costs(x = NULL)
.check_tzones(...)
.decode_numeric_version(x)
.encode_numeric_version(x) 
.make_numeric_version(x, strict = TRUE, regexp, classes = NULL) 
.expand_R_libs_env_var(x) 
.kronecker(X, Y, FUN = "*", make.dimnames = FALSE, ...) 
.packageStartupMessage(message, call = NULL) 
.rmpkg(pkg) 
.formula2varlist(formula, data, warnLHS = TRUE, ignoreLHS = warnLHS)
.set_ops_need_as_vector(x, y)
}
\arguments{
  \item{x}{an object
    \describe{
      \item{for \code{.subset*()}:}{from which to extract elements.}
      \item{for \code{.row_names_info()}:}{with a \code{"row.names"} attribute, typically a
	data frame.}
      \item{for \code{.gt}, \code{.gtn}:}{typically S3- or S4-classed.}
    }
  }
  \item{file}{\describe{
      \item{for \code{.getRequiredPackages()}:}{the full path to the
	\file{DESCRIPTION} file of a package.}
      \item{for \code{lazyLoadDBfetch()}:}{the filepath to a lazyload database.}
    }
  }
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search through, or \code{NULL}.  The default value
    of \code{NULL} corresponds to all libraries currently known to
    \code{\link{.libPaths}()}.}
  \item{quiet}{logical.  Should this not give warnings or an error
    if the package(s) are not found?}
  \item{useImports}{logical.  Should  the \code{Imports} field be processed?}
  \item{quietly}{logical: should message(s) be printed.}
  \item{pkgInfo}{a processed \file{DESCRIPTION} file of class
    \code{"packageDescription2"}.}
  %% \item{built, run}{platform strings of the form \code{"cpu-vendor-os"}.}
  \item{name}{name of image file to save or load.}
  \item{type}{integer.  Currently \code{type = 0} returns the internal
    \code{"row.names"} attribute (possibly \code{NULL}), \code{type = 2}
    the number of rows implied by the attribute, and \code{type = 1} the
    latter with a negative sign for \sQuote{automatic} row names.}
  \item{n}{integer.  The number of rows.}
  \item{info}{character string such as \code{"package:MASS"}.}
  \item{key}{the object name in a lazyload database.}
  \item{compressed}{logical: are the values in the database compressed?}
  \item{hook}{a load hook: see \code{\link{unserialize}}.}
  \item{seed}{a length-one integer vector.}
  \item{i, j}{indices.}
  \item{strictly}{logical.}
  \item{obj}{an \R object.}
  \item{xx}{a classless object to be placed into the class.}
  \item{tz}{an optional time zone.}
  \item{cl}{an optional character vector of classes \emph{containing}
    the default classes.}
  \item{units}{see \code{\link{difftime}}.}
  \item{\dots}{arguments passed to non-dot name versions.}
  \item{pos}{an integer.}

  \item{same}{character vector of \emph{same} objects, masking others.}
  \item{pkg}{character string naming the package which is masked from or by.}
  \item{by}{logical indicating if the masking happens \emph{by}
    \code{pkg}, or (\code{by = FALSE}) from \code{pkg}.}
  \item{formula}{typically a one sided formula, with no LHS.}
  \item{data}{a data frame where terms in the formula are to be evaluated.}
  \item{warnLHS}{logical flag, indicating whether the presence of an LHS
    should trigger a warning.}
  \item{ignoreLHS}{logical flag, indicating whether variables in the LHS
    should be ignored.}
}
\details{
  The functions \code{.subset} and \code{.subset2} are essentially
  equivalent to the \code{\link{[}} and \code{\link{[[}} operators,
  except that methods dispatch does not take place.  This is to avoid
  expensive \I{unclassing} when applying the default method to an object.  They
  should not normally be invoked by end users.  Note that unlike the
  operators they are builtins and not specials (all arguments are
  evaluated) and hence do not allow missing arguments.

  \code{.getRequiredPackages2} attaches all the packages mentioned in the
  \code{Depends} field: failure to find a package is an error.  It also
  checks the versions of the packages found against the \code{Depends} field.

  \code{.getRequiredPackages} is a wrapper to
  \code{.getRequiredPackages2} using a \file{DESCRIPTION} file.

%%   \code{testPlatformEquivalence} is used to test if a package with
%%   compiled code can safely be loaded into a build of \R, which it does by
%%   comparing their \sQuote{canonical} \code{"cpu-vendor-os"} descriptions.
%%   The default function omits the \code{vendor} part and allows small
%%   mismatches elsewhere.

  The function \code{.isMethodsDispatchOn()} returns \code{TRUE} if
  the S4 method dispatch has been turned on in the evaluator (usually by
  loading package \pkg{methods}).  It is meant for \R internal use only.

  \code{sys.save.image} is a system function that is called by \code{q()}
  and its GUI analogs; \code{sys.load.image} is called by the startup code.
  These functions should not be called directly and are subject to change.

  \code{sys.save.image} closes all connections first, to ensure that it
  is able to open a connection to save the image.  This is appropriate
  when called from \code{q()} and allies, but reinforces the warning
  that it should not be called directly.

  \code{\link{row.names}} can be stored internally in compact form.
  \code{.set_row_names(n)} generates that form for automatic row names
  of length \code{n}, to be assigned to
  \code{attr(<a data frame>, "row.names")}.  \code{.row_names_info}
  gives information on the internal form of the row names for a data
  frame: for details of what information see the argument \code{type}.

  \code{.GenericArgsEnv} and \code{.ArgsEnv} are environments that
  contain closures with the argument lists that the primitives would
  have had had they been closures.  All the primitives that are internal
  S3 generics have corresponding members of \code{.GenericArgsEnv} and
  the remaining non-language-element primitives correspond to
  \code{.ArgsEnv}.  See the \sQuote{R Internals} manual for further
  details.  They are used by \code{\link{args}} and
  \code{\link{print.default}} and the QC functions \code{\link{codoc}}
  and \code{\link{checkS3methods}}.

  \code{findPackageEnv} is invoked by the unserialize code to set a
  saved environment if possible.

  \code{.TAOCP1997init} is the initialization code for the
  \code{"Knuth-TAOCP"} \link{RNG}.

  \code{.gt} and \code{.gtn} are callbacks from \code{\link{rank}} and
  \code{\link{is.unsorted}} used for (S3 or S4) classed objects.

  \code{.primTrace} and \code{.primUntrace} are the \link{primitive}
  functions underlying \code{\link{trace}} and \code{\link{untrace}}
  respectively.

  \code{.Date}, \code{.POSIXct}, \code{.POSIXlt} and \code{.difftime}
  are class generators.

  \code{.cache_class} caches the inheritance of an S4 class for use in
  S3 method dispatch.  With \code{NULL} second argument it returns the
  cached inheritance, for diagnostic use.

  \code{.popath} is a character string created at startup which records where
  the \pkg{translations} package in use is, possibly \code{NA_character_}.

  \code{.detach} is a \sQuote{bare-bones} version of \code{\link{detach}} for
  use in other \R packages.

  \code{.maskedMsg} is a utility called both from \code{\link{attach}()}
  and \code{\link{library}()} for consistency to produce the warning message.

  Objects starting \code{.C_} and \code{.F_} are references to
  registered C and Fortran entry points.

  \bold{Only on Windows}:\cr
  \code{.fixupGFortranStdout} and \code{.fixupGFortranStderr} are helper functions
  to enable standard output and standard error units in \code{gfortran} when
  executing external code via \code{system} and \code{system2}. By default, these
  units are disabled by the Windows profile when running inside \I{RGui}.
}
\value{
  \code{.getRequiredPackages} and \code{.getRequiredPackages2}
  return \code{invisible()}.

  %% \code{testPlatformEquivalence} returns \code{TRUE} or \code{FALSE}.

  \code{.formula2varlist} returns a list of variables found in
  \code{formula}, evaluated in \code{data}, following standard
  non-standard evaluation rules. Used in \code{\link{split}} and
  \code{\link{tapply}} to interpret a formula as a list of categorical
  variables to split on.

  \code{.set_ops_need_as_vector} is a helper function for the set
  operations like \code{\link{union}}: it returns \code{TRUE} if the set
  ops need to transform their arguments via \code{\link{as.vector}}.
}
\keyword{internal}
