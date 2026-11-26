% File src/library/stats/man/model.extract.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2026 R Core Team
% Distributed under GPL 2 or later

\name{model.extract}
\title{Extract Components from a Model Frame}
\usage{
model.extract(frame, component)
model.offset(x)
model.response(data, type = "any")
model.weights(x)
}
\alias{model.extract}
\alias{model.offset}
\alias{model.response}
\alias{model.weights}
\arguments{
 \item{frame, x, data}{a model frame, see \code{\link{model.frame}}.}
 \item{component}{literal character string.  The name of a
   component to extract, such as \code{"weights"} or \code{"etastart"}.
   Passing a name (unquoted), e.g., \code{weights}, is deprecated.}
 \item{type}{One of \code{"any"}, \code{"numeric"} or \code{"double"}.
   Using either of latter two coerces the result to have storage mode
   \code{"double"}.}
}
\description{Returns the response, offset, weights or other
  special components of a model frame passed as optional arguments to
  \code{\link{model.frame}}.
}
\details{
  \code{model.extract} is provided for compatibility with S, which does
  not have the more specific functions.  It is also useful to extract
  e.g.\sspace{}the \code{etastart} and \code{mustart} components of a
  \code{\link{glm}} fit.

  For S compatibility, \code{component} could also be given as a name
  (unquoted), as in \code{model.extract(m, weights)}; this is deprecated,
  not least because it gives a \dQuote{no visible binding for global
  variable} \sQuote{NOTE} from \command{R CMD check} in package code.
  Use a character string, or for the three components below, the
  specific function.

  \code{model.extract(m, "offset")}, \code{model.extract(m, "response")}
  and \code{model.extract(m, "weights")} are equivalent to
  \code{model.offset(m)}, \code{model.response(m)} and
  \code{model.weights(m)} respectively, apart from naming, see below.
  \code{model.offset} sums any terms specified by
  \code{\link{offset}} terms in the formula or by \code{offset} arguments
  in the call producing the model frame: it does check that the offset
  is numeric.

  \code{model.weights} is slightly different from
  \code{model.extract(, "weights")} in not naming the vector it returns.
}
\value{The specified component of the model frame, usually a vector.
  \code{model.response()} now \emph{drops} a possible \code{"Asis"} class
  (stemming from \code{\link{I}(.)}).

  \code{model.offset} returns \code{NULL} if no offset was specified.
}

\seealso{\code{\link{model.frame}}, \code{\link{offset}}}

\examples{
a <- model.frame(cbind(ncases,ncontrols) ~ agegp + tobgp + alcgp, data = esoph)
             model.extract(a, "response")
   stopifnot(model.extract(a, "response") == model.response(a))
tools::assertWarning(verbose = TRUE, ## "passing a bare name .... is deprecated."
   stopifnot(model.extract(a, response) == model.response(a)))

a <- model.frame(ncases/(ncases+ncontrols) ~ agegp + tobgp + alcgp,
                 data = esoph, weights = ncases+ncontrols)
model.response(a)
(mw <- model.extract(a, "weights"))
stopifnot(identical(unname(mw), model.weights(a)))

a <- model.frame(cbind(ncases,ncontrols) ~ agegp,
                 something = tobgp, data = esoph)
names(a) # .. .. "(something)"
stopifnot(model.extract(a, "something") == esoph$tobgp)
     all (model.extract(a,  something ) == esoph$tobgp) # TRUE (w/ deprecation warning)
}
\keyword{manip}
\keyword{programming}
\keyword{models}
