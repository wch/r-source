\name{Deprecated}
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions}
\alias{Deprecated}
\alias{.Deprecated}
\alias{print.coefmat}
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
% </Deprecated 1.8.0>
}
\details{
  \code{.Deprecated("<new name>")} is called from deprecated
  functions. The original help page for these functions is often
  available at \code{help("oldName-deprecated")} (note the quotes).

  \code{print.coefmat} is an older name for \code{\link{printCoefmat}}
  with a different default for \code{na.print}.
}
\seealso{
  \code{\link{Defunct}},
}
\keyword{documentation}
\keyword{utilities}
