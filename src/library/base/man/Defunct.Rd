\name{Defunct}
\title{Defunct Functions}
\alias{Defunct}
\alias{.Defunct}
%------ PLEASE: put \alias{.} here for  EACH !
\alias{Version}
\alias{provide}
\alias{.Provided}
\alias{category}
\alias{dnchisq}
\alias{pnchisq}
\alias{qnchisq}
\alias{rnchisq}
\alias{print.anova.glm}
\alias{print.anova.lm}
\alias{print.tabular}
\alias{print.plot}
\alias{save.plot}
\alias{system.test}
\alias{dotplot}
\alias{stripplot}
\alias{getenv}
\alias{read.table.url}
\alias{scan.url}
\alias{source.url}
\alias{httpclient}
\alias{parse.dcf}
\alias{.Alias}
\alias{reshapeLong}
\alias{reshapeWide}
\alias{piechart}
\alias{print.ordered}
\alias{.Dyn.libs}
\alias{.lib.loc}
\alias{machine}
\alias{Platform}
\alias{Machine}
\alias{restart}
%
\description{
  The functions or variables listed here are no longer part of \R as
  they are not needed (any more).
}
\usage{
.Defunct()

%--- oldfun(.)
Version()
provide(package)
% Removed in 1.2.0
.Provided
category(\dots)
dnchisq(.)
pnchisq(.)
qnchisq(.)
rnchisq(.)
print.anova.glm(.)
print.anova.lm(.)
print.tabular(.)
print.plot(.)
save.plot(.)
system.test(.)
% </Removed 1.2.0>
% Removed in 1.3.0
dotplot(\dots)
stripplot(\dots)
getenv(\dots)
% </Removed 1.3.0>
% Removed in 1.4.0
read.table.url(url, method,\dots)
scan.url(url, file = tempfile(), method, \dots)
source.url(url, file = tempfile(), method, \dots)
httpclient(url, port=80, error.is.fatal=TRUE, check.MIME.type=TRUE,
           file=tempfile(), drop.ctrl.z=TRUE)
parse.dcf(text = NULL, file = "", fields = NULL, versionfix = FALSE)
% </Removed 1.4.0>
% Removed in 1.5.0
.Alias(expr)
reshapeWide(x, i = reshape.i, j = reshape.j, val = reshape.v,
            jnames = levels(j))
reshapeLong(x,jvars,  ilev = row.names(x),
            jlev = names(x)[jvars], iname = "reshape.i",
            jname = "reshape.j", vname = "reshape.v")
% </Removed 1.5.0>
% Removed in 1.6.0
piechart(x, labels = names(x), edges = 200, radius = 0.8,
         density = NULL, angle = 45, col = NULL, main = NULL, ...)
print.ordered(.)
.Dyn.libs
.lib.loc
% </Removed 1.6.0>
% Removed in 1.7.0
machine()
Machine()
Platform()
restart()
% </Removed 1.7.0>
}
\details{
  \code{.Defunct} is the function to which defunct functions are set.
  
  \code{category} has been an old-S function before there were factors;
  should be replaced by \code{\link{factor}} throughout!

  The \code{*chisq()} functions now take an optional non-centrality
  argument, so the \code{*nchisq()} functions are no longer needed.

  The new function \code{dev.print()} should now be used for saving
  plots to a file or printing them.

  \code{provide} and its object \code{.Provided} have been removed.
  They were never used for their intended purpose, to allow one
  package to subsume another.

  \code{dotplot} and \code{stripplot} have been renamed to
  \code{\link{dotchart}} and \code{\link{stripchart}}, respectively.

  \code{getenv} has been replaced by \code{\link{Sys.getenv}}.


  \code{*.url} are replaced by calling \code{read.table}, \code{scan} or
  \code{source} on a \code{\link{url}} connection.

  \code{httpclient} was used by the deprecated \code{"socket"} method
  of \code{\link{download.file}}.

  \code{parse.dcf} has been replaced by \code{read.dcf}, which is much
  faster, but has a slightly different interface.

  \code{.Alias} provided an unreliable way to create duplicate
  references to the same object. There is no direct replacement. Where
  multiple references to a single object are required for semantic
  reasons consider using environments or external pointers. There are
  some notes on \url{http://developer.r-project.org}.

  \code{reshape*}, which were experimental, are replaced by
  \code{\link{reshape}}. This has a different syntax and allows multiple
  time-varying variables.

  \code{piechart} is the old name for \code{pie}, but clashed with usage
  in Trellis.

  \code{.Dyn.libs} and \code{.lib.loc} were internal variables used for
  storing and manipulating the information about packages with dynloaded
  shared libs, and the known \R library trees.  These are now dynamic
  variables which one can get or set using \code{\link{.dynLibs}} and
  \code{\link{.libPaths}}, respectively.

  \code{Machine()} and \code{Platform()} were functions returning the
  variables \code{\link{.Machine}} and \code{\link{.Platform}}
  respectively.

  \code{restart()} should be replaced by \code{try()}, in preparation
  for an exception-based implementation.  If you use \code{restart()} in
  a way that cannot be replaced with \code{try()} then ask for help
  on \code{r-devel}.
}
\seealso{\code{\link{Deprecated}}}
\keyword{documentation}
\keyword{utilities}
\keyword{internal}
