% File src/library/base/man/base-defunct.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{base-defunct}
\alias{base-defunct}
\title{Defunct Functions in Package \pkg{base}}
%------ PLEASE: put \alias{.} here for  EACH !
\alias{Version}
\alias{provide}
\alias{.Provided}
\alias{category}
\alias{print.anova.glm}
\alias{print.anova.lm}
\alias{print.tabular}
\alias{print.plot}
\alias{save.plot}
\alias{system.test}
\alias{getenv}
\alias{read.table.url}
\alias{scan.url}
\alias{source.url}
\alias{httpclient}
\alias{parse.dcf}
\alias{.Alias}
\alias{print.ordered}
\alias{.Dyn.libs}
\alias{.lib.loc}
\alias{machine}
\alias{Platform}
\alias{Machine}
\alias{restart}
\alias{printNoClass}
\alias{codes}
\alias{codes.factor}
\alias{codes.ordered}
\alias{codes<-}
\alias{print.atomic}
\alias{La.eigen}
\alias{tetragamma}
\alias{pentagamma}
\alias{package.description}
\alias{loadURL}
\alias{delay}
\alias{write.table0}
\alias{format.char}
\alias{La.chol}
\alias{La.chol2inv}
\alias{symbol.C}
\alias{symbol.For}
\alias{unix}
\alias{gammaCody}
\alias{manglePackageName}
\alias{.Import}
\alias{.ImportFrom}
\alias{.Export}
\alias{.S3method}
\alias{.First.lib}
\alias{mem.limits}
\alias{.readRDS}
\alias{.saveRDS}
\alias{Sys.putenv}
\alias{real}
\alias{as.real}
\alias{is.real}
\alias{.find.package}
\alias{.path.package}
%
\description{
  The functions or variables listed here are no longer part of \R as
  they are no longer needed.
}
\usage{
# Defunct in 1.x
Version()
provide(package)
.Provided
category(\dots)
print.anova.glm(.)
print.anova.lm(.)
print.tabular(.)
print.plot(.)
save.plot(.)
system.test(.)
getenv(\dots)
read.table.url(url, method, \dots)
scan.url(url, file = tempfile(), method, \dots)
source.url(url, file = tempfile(), method, \dots)
httpclient(url, port = 80, error.is.fatal = TRUE,
           check.MIME.type = TRUE, file = tempfile(),
           drop.ctrl.z = TRUE)
parse.dcf(text = NULL, file = "", fields = NULL,
          versionfix = FALSE)
.Alias(expr)
print.ordered(.)
.Dyn.libs
.lib.loc
machine()
Machine()
Platform()
restart()
printNoClass(x, digits = NULL, quote = TRUE, na.print = NULL,
             print.gap = NULL, right = FALSE, \dots)
codes(x, \dots)
codes(x, \dots) <- value

# Defunct in 2.0.0
La.eigen(x, symmetric, only.values = FALSE,
         method = c("dsyevr", "dsyev"))
tetragamma(x)
pentagamma(x)
package.description(pkg, lib.loc = NULL, fields = NULL)

# Defunct in 2.2.0
loadURL(url, envir = parent.frame(), quiet = TRUE, \dots)
delay(x, env = .GlobalEnv)

# Defunct in 2.3.0
format.char(x, width = NULL, flag = "-")

# Defunct in 2.4.0
La.chol(x)
La.chol2inv(x, size = ncol(x))

# Defunct in 2.4.0
symbol.C(name)
symbol.For(name)

# Defunct in 2.5.0
unix(call, intern = FALSE)

# Defunct in 2.8.0
gammaCody(x)

# Defunct in 2.9.0
manglePackageName(pkgName, pkgVersion)

# Defunct in 2.13.0
.Import(...)
.ImportFrom(name, ...)
.Export(...)
.S3method(generic, class, method)

# Defunct in 2.14.0
.First.lib(lib, pkg)

# Defunct in 2.15.0
mem.limits(nsize = NA, vsize = NA)
.saveRDS(\dots)
.readRDS(\dots)
Sys.putenv(\dots)

# Defunct in 3.0.0
real(length = 0)
as.real(x, \dots)
is.real(x)

# Defunct in 3.1.0
.find.package(\dots)
.path.package(\dots)
}
\details{
  Some of these have stubs which report that they are defunct, but most
  have been removed completely (apart from being documented here).
  
  \code{category} has been an old-S function before there were factors;
  should be replaced by \code{\link{factor}} throughout!

  The new function \code{dev.print()} should now be used for saving
  plots to a file or printing them.

  \code{provide} and its object \code{.Provided} have been removed.
  Their intended purpose was to allow one package to subsume another,
  but this was never used.

  \code{getenv} has been replaced by \code{\link{Sys.getenv}}.

  \code{*.url} are replaced by calling \code{read.table}, \code{scan} or
  \code{source} on a \code{\link{url}} connection.

  \code{httpclient} was used by the deprecated \code{"socket"} method
  of \code{\link{download.file}}.

  \code{parse.dcf} has been replaced by \code{read.dcf}, which is much
  faster, but has a slightly different interface.

  \code{.Alias} provided an unreliable way to create duplicate
  references to the same object.  There is no direct replacement.  Where
  multiple references to a single object are required for semantic
  reasons consider using environments or external pointers.  There are
  some notes on \url{http://developer.r-project.org}.

  \code{.Dyn.libs} and \code{.lib.loc} were internal variables used for
  storing and manipulating the information about packages with DLLs, and
  the known \R library trees.  These are now dynamic variables which one
  can get or set using \code{\link{.dynLibs}} and
  \code{\link{.libPaths}}, respectively.

  \code{Machine()} and \code{Platform()} were functions returning the
  variables \code{\link{.Machine}} and \code{\link{.Platform}}
  respectively.

  \code{restart()} should be replaced by \code{try()}, in preparation
  for an exception-based implementation.  If you use \code{restart()} in
  a way that cannot be replaced with \code{try()} then ask for help
  on \code{r-devel}.

  \code{printNoClass} was in package \pkg{methods} and called directly the
  internal function \code{print.default}.

  \code{codes} was almost always used inappropriately.  To get the
  internal coding of a factor, use \code{unclass}, \code{as.vector} or
  \code{as.integer}.  For \emph{ordered} factors, \code{codes} was
  equivalent to these, but for \emph{unordered} factors it assumed an
  alphabetical ordering of the levels in the locale in use.

  \code{La.eigen} has become the default for \code{\link{eigen}}.

  \code{tetragamma} and \code{pentagamma} have been replaced by
  \code{\link{psigamma}}.

  \code{package.description} has been replaced by
  \code{\link{packageDescription}} in package \pkg{utils}.

  \code{loadURL} has been superseded by \code{\link{load}(url())}.

  \code{delay} has been replaced by \code{\link{delayedAssign}}.
  Unevaluated promises should never be visible.

  \code{write.table0} was an \R-level version of \code{write.table}
  provided for temporary back-compatibility.

  \code{format.char} was an auxiliary function for \code{\link{formatC}}
  whose functionality (and more) is provided by \code{\link{format.default}}.

  \code{La.chol} and \code{La.chol2inv} became the default methods for
  \code{\link{chol}} and \code{\link{chol2inv}}.

  Functions \code{symbol.C} and \code{symbol.For} were provided for
  historical compatibility with S.  \code{symbol.C} just returns its
  argument unchanged, whereas \code{symbol.For} will map to lower case
  and append an underscore on those platforms (almost all) which do so
  for Fortran symbols.  These are no longer of any use in \R.

  \code{unix} was a \emph{deprecated} (since the 1990s) alternative to
  \code{\link{system}}, available for backwards compatibility with S.

  \code{gammaCody} was the \eqn{(\Gamma)} function from the Specfun
  package used at C level in the Bessel code. The \R-level interface was
  here for experimental purposes.

  \code{manglePackageName} was part of the defunct support for versioned
  installs.

  \code{.Import}, \code{.ImportFrom}, \code{.Export} and
  \code{.S3method} were part of an experimental alternative way to
  specify namespaces.

  \code{.First.lib} was the load/attach hook for packages without
  namespaces: use \code{\link{.onLoad}} or \code{\link{.onAttach}} instead.

  \code{mem.limits} reported on the old-style limits on the number of
  cons cells and size of the vector heap.  These are no longer limited
  except by overall memory limits.

  \code{.readRDS} and \code{.saveRDS}, were internal-only versions (and
  the only versions available prior to \R 2.13.0) of
  \code{\link{.readRDS}} and \code{\link{.saveRDS}}.

  \code{Sys.putenv} was a synonym for \code{\link{Sys.setenv}},
  deprecated since \R 2.5.0.

  \code{real}, \code{as.real} and \code{is.real} were long-deprecated
  synonyms for the \code{double} versions.

  \code{.find.package} and \code{.path.package} were internal-only
  versions prior to \R 2.13.0, and since deprecated wrappers for
  \code{\link{find.package}} and \code{\link{path.package}}.
}
\seealso{
  \code{\link{Defunct}}, \code{\link{base-deprecated}}
}

\keyword{internal}
