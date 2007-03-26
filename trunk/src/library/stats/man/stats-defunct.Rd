\name{stats-defunct}
%------ NOTE:  ../R/stats-defunct.R   must be synchronized with this!
\alias{stats-defunct}
\title{Defunct Functions in Package stats}
%------ PLEASE: put \alias{.} here for  EACH !
\alias{dnchisq}
\alias{pnchisq}
\alias{qnchisq}
\alias{rnchisq}
\alias{reshapeLong}
\alias{reshapeWide}
\alias{plot.mts}
\alias{print.coefmat}
\alias{anovalist.lm}
\alias{lm.fit.null}
\alias{lm.wfit.null}
\alias{glm.fit.null}
\alias{mauchley.test}
%
\description{
  The functions or variables listed here are no longer part of \R as
  they are not needed (any more).
}
\usage{
%--- oldfun(.)
% Removed in 1.2.0
dnchisq(.)
pnchisq(.)
qnchisq(.)
rnchisq(.)
print.anova.glm(.)
print.anova.lm(.)
% </Removed 1.2.0>
% Removed in 1.5.0
reshapeWide(x, i, j, val, jnames = levels(j))
reshapeLong(x,jvars,  ilev = row.names(x),
            jlev = names(x)[jvars], iname = "reshape.i",
            jname = "reshape.j", vname = "reshape.v")
% </Removed 1.5.0>
% Removed in 1.8.0
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

anovalist.lm(object, \dots, test = NULL)
lm.fit.null(x, y, method = "qr", tol = 1e-07, \dots)
lm.wfit.null(x, y, w, method = "qr", tol = 1e-07, \dots)
glm.fit.null(x, y, weights, start = NULL,
             etastart = NULL, mustart = NULL, offset,
             family = gaussian(), control = glm.control(),
             intercept = FALSE)
% </Removed 1.9.0>
% <Removed 2.4.0>
mauchley.test(object, Sigma = diag(nrow = p),
   T = Thin.row(proj(M) - proj(X)), M = diag(nrow = p), X = ~0,
   idata = data.frame(index = seq(length = p)), ...)
% </Removed 2.4.0>
}
\details{
  The \code{*chisq()} functions now take an optional non-centrality
  argument, so the \code{*nchisq()} functions are no longer needed.

  \code{reshape*}, which were experimental, are replaced by
  \code{\link{reshape}}.  This has a different syntax and allows
  multiple time-varying variables.

  \code{plot.mts} has been removed, as \code{\link{plot.ts}} now has the
  same functionality.

  \code{print.coefmat} was an older name for \code{\link{printCoefmat}}
  with a different default for \code{na.print}.

  \code{anovalist.lm} was replaced by \code{\link{anova.lmlist}} in
  \R 1.2.0.

  \code{lm.fit.null} and \code{lm.wfit.null} are superseded by
  \code{lm.fit} and \code{lm.wfit} which handle null models now.
  Similarly, \code{glm.fit.null} is superseded by \code{glm.fit}.

  \code{mauchley.test} was a misspelling of Mauchly's name, corrected
  by the introduction of \code{\link{mauchly.test}}.
}
\seealso{
  \code{\link{Defunct}}
}
\keyword{internal}
