\name{Deprecated}
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions}
\alias{Deprecated}
\alias{.Deprecated}
\alias{tkfilefind}
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
%------ PLEASE: put \alias{.} here for EACH !
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as of the next release.
}
\usage{
.Deprecated(new)

% Deprecated in 1.8.0
print.coefmat(x, digits=max(3, getOption("digits") - 2),
              signif.stars = getOption("show.signif.stars"),
              dig.tst = max(1, min(5, digits - 1)),
              cs.ind = 1:k, tst.ind = k + 1, zap.ind = integer(0),
              P.values = NULL,
              has.Pvalue = nc >= 4 && substr(colnames(x)[nc],1,3) == "Pr(",
              eps.Pvalue = .Machine$double.eps,
              na.print = "", \dots)

codes(x, \dots)
codes(x, \dots) <- value

anovalist.lm(object, \dots, test = NULL)
lm.fit.null(x, y, method = "qr", tol = 1e-07, \dots)
lm.wfit.null(x, y, w, method = "qr", tol = 1e-07, \dots)
glm.fit.null(x, y, weights = rep(1, nobs), start = NULL,
             etastart = NULL, mustart = NULL, offset = rep(0, nobs),
             family = gaussian(), control = glm.control(),
             intercept = FALSE)
print.atomic(x, quote = TRUE, \dots)
% </Deprecated 1.8.0>
}
\details{
  \code{.Deprecated("<new name>")} is called from deprecated
  functions. The original help page for these functions is often
  available at \code{help("oldName-deprecated")} (note the quotes).

  \code{tkfilefind} is a demo in package \pkg{tcltk} displaying a
  widget for selecting files but the same functionality is available in
  a better form in the \code{tkgetOpenFile} and \code{tkgetSaveFile}
  functions. The demo is reported not even to work with recent versions
  of Tcl and Tk libraries.

  \code{print.coefmat} is an older name for \code{\link{printCoefmat}}
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

  \code{print.atomic} differs from \code{print.default} only in its
  argument sequence.  It is not a method for \code{print}.
}
\seealso{
  \code{\link{Defunct}},
}
\keyword{documentation}
\keyword{utilities}
