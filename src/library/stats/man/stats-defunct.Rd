% File src/library/stats/man/stats-defunct.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{stats-defunct}
%------ NOTE:  ../R/stats-defunct.R   must be synchronized with this!
\alias{stats-defunct}
\title{Defunct Functions in Package \pkg{stats}}
%------ PLEASE: put \alias{.} here for  EACH !
\alias{arima0.diag}
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
\alias{clearNames}
%
\description{
  The functions or variables listed here are no longer part of \R as
  they are not needed (any more).
}
\usage{
## Defunct in R 1.x
dnchisq(.)
pnchisq(.)
qnchisq(.)
rnchisq(.)
print.anova.glm(.)
print.anova.lm(.)
reshapeWide(x, i, j, val, jnames = levels(j))
reshapeLong(x, jvars,  ilev = row.names(x),
            jlev = names(x)[jvars], iname = "reshape.i",
            jname = "reshape.j", vname = "reshape.v")
arima0.diag(\dots)
plot.mts(x, plot.type = c("multiple", "single"), panel = lines,
         log = "", col = par("col"),  bg = NA, pch = par("pch"),
         cex = par("cex"), lty = par("lty"), lwd = par("lwd"),
         ann = par("ann"),  xlab = "Time", type = "l", main = NULL,
         oma = c(6, 0, 5, 0), \dots)
print.coefmat(x, digits = max(3, getOption("digits") - 2),
              signif.stars = getOption("show.signif.stars"),
              dig.tst = max(1, min(5, digits - 1)),
              cs.ind, tst.ind, zap.ind = integer(0),
              P.values = NULL, has.Pvalue,
              eps.Pvalue = .Machine$double.eps,
              na.print = "", \dots)
anovalist.lm(object, \dots, test = NULL)
lm.fit.null(x, y, method = "qr", tol = 1e-07, \dots)
lm.wfit.null(x, y, w, method = "qr", tol = 1e-07, \dots)
glm.fit.null(x, y, weights, start = NULL,
             etastart = NULL, mustart = NULL, offset,
             family = gaussian(), control = glm.control(),
             intercept = FALSE)

# Defunct in 2.4.0
mauchley.test(object, Sigma = diag(nrow = p),
   T = Thin.row(proj(M) - proj(X)), M = diag(nrow = p), X = ~0,
   idata = data.frame(index = seq(length = p)), ...)

# Defunct in 2.10.0
clearNames(object)
}
\details{
  The \code{*chisq()} functions now take an optional non-centrality
  argument, so the \code{*nchisq()} functions are no longer needed.

  \code{reshape*}, which were experimental, are replaced by
  \code{\link{reshape}}.  This has a different syntax and allows
  multiple time-varying variables.

  \code{arima0.diag} has been replaced by \code{\link{tsdiag.arima0}}.
  
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

  \code{clearNames} had been introduced at about the same time as
  \code{\link{unname}}, but is less general and has been used rarely.
}
\seealso{
  \code{\link{Defunct}}
}
\keyword{internal}
