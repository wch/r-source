\name{base-defunct}
\alias{base-defunct}
\title{Defunct Functions in Base Package}
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
\alias{printNoClass}
\alias{plot.mts}
\alias{print.coefmat}
\alias{codes}
\alias{codes.factor}
\alias{codes.ordered}
\alias{codes<-}
\alias{anovalist.lm}
\alias{lm.fit.null}
\alias{lm.wfit.null}
\alias{glm.fit.null}
\alias{print.atomic}
\alias{La.eigen}
\alias{tetragamma}
\alias{pentagamma}
\alias{package.description}
%
\description{
  The functions or variables listed here are no longer part of \R as
  they are not needed (any more).
}
\usage{
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
reshapeWide(x, i, j, val, jnames = levels(j))
reshapeLong(x,jvars,  ilev = row.names(x),
            jlev = names(x)[jvars], iname = "reshape.i",
            jname = "reshape.j", vname = "reshape.v")
% </Removed 1.5.0>
% Removed in 1.6.0
piechart(x, labels = names(x), edges = 200, radius = 0.8,
         density = NULL, angle = 45, col = NULL, main = NULL, \dots)
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
% Removed in 1.8.0
printNoClass(x, digits = NULL, quote = TRUE, na.print = NULL,
             print.gap = NULL, right = FALSE, \dots)
% plot.mts can't have such an object, though
plot.mts(x, plot.type = c("multiple", "single"), panel = lines,
         log = "", col = par("col"),  bg = NA, pch = par("pch"),
         cex = par("cex"), lty = par("lty"), lwd = par("lwd"),
         ann = par("ann"),  xlab = "Time", type = "l", main=NULL,
         oma=c(6, 0, 5, 0), \dots)
% </Removed 1.8.0>
% Removed in 1.9.0
print.coefmat(x, digits=max(3, getOption("digits") - 2),
              signif.stars = getOption("show.signif.stars"),
              dig.tst = max(1, min(5, digits - 1)),
              cs.ind, tst.ind, zap.ind = integer(0),
              P.values = NULL, has.Pvalue, eps.Pvalue = .Machine$double.eps,
              na.print = "", \dots)

codes(x, \dots)
codes(x, \dots) <- value

anovalist.lm(object, \dots, test = NULL)
lm.fit.null(x, y, method = "qr", tol = 1e-07, \dots)
lm.wfit.null(x, y, w, method = "qr", tol = 1e-07, \dots)
glm.fit.null(x, y, weights, start = NULL,
             etastart = NULL, mustart = NULL, offset,
             family = gaussian(), control = glm.control(),
             intercept = FALSE)
print.atomic(x, quote = TRUE, \dots)
% </Removed 1.9.0>
% Removed in 2.0.0
La.eigen(x, symmetric, only.values = FALSE, method = c("dsyevr", "dsyev"))
tetragamma(x)
pentagamma(x)
package.description(pkg, lib.loc = NULL, fields = NULL)
% </Removed 2.0.0>
}
\details{
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
  \code{\link{dotchart}} and \code{\link{stripchart}},
  respectively.

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
  \code{\link{reshape}}.  This has a different syntax and allows
  multiple time-varying variables.

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

  \code{printNoClass} was in package \pkg{methods} and calls directly the
  internal function \code{print.default}.

  \code{plot.mts} has been removed, as \code{\link{plot.ts}} now has the
  same functionality.

  \code{print.coefmat} was an older name for \code{\link{printCoefmat}}
  with a different default for \code{na.print}.

  \code{codes} was almost always used inappropriately.  To get the
  internal coding of a factor, use \code{unclass}, \code{as.vector} or
  \code{as.integer}.  For \emph{ordered} factors, \code{codes} was
  equivalent to these, but for \emph{unordered} factors it assumed an
  an alphabetical ordering of the levels in the locale in use.

  \code{anovalist.lm} was replaced by \code{\link{anova.lmlist}} in
  \R 1.2.0.

  \code{lm.fit.null} and \code{lm.wfit.null} are superseded by
  \code{lm.fit} and \code{lm.wfit} which handle null models now.
  Similarly, \code{glm.fit.null} is superseded by \code{glm.fit}.

  \code{print.atomic} differed from \code{print.default} only in its
  argument sequence.  It is not a method for \code{print}.

  \code{La.eigen} has become the default for \code{\link{eigen}}.

  \code{tetragamma} and \code{pentagamma} have been replaced by
  \code{\link{psigamma}}.

  \code{package.description} has been replaced by
  \code{\link{packageDescription}}.
}
\seealso{\code{\link{Defunct}}, \code{\link{base-deprecated}}}
\keyword{internal}
