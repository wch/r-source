% File src/library/base/man/factor.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{droplevels}
\title{droplevels}
\alias{droplevels}
\alias{droplevels.factor}
\alias{droplevels.data.frame}
\concept{categorical variable}
\concept{enumerated type}
\concept{category}
\description{
  The function \code{droplevels} is used to drop unused levels from a factor or, more commonly, from factors in a data frame.
}
\usage{
\S3method{droplevels}{factor}(x, \dots)
\S3method{droplevels}{data.frame}(x, except, \dots)
}
\arguments{
  \item{x}{an object from which to drop unused factor levels.}
  \item{\dots}{further arguments passed to methods}
  \item{except}{indices of columns from which \emph{not} to drop levels}
}
\value{
  \code{droplevels} returns an object of the same class as \code{x}}
\details{
  The method for class \code{"factor"} is essentially equivalent to \code{factor(x)}.

  The \code{except} argument follow the usual indexing rules.
}

\note{ This function was introduced in R 2.12.0. It is primarily
  intended for cases where one or more factors in a data frame
  contains only elements from a reduced level set after
  subsetting.  (Notice that subsetting does \emph{not} in general drop
  unused levels).  By default, levels are dropped from all factors in a
  data frame, but the \code{except} argument allows you to specify
  columns for which this is not wanted.
}

\seealso{
  \code{\link{subset}} for subsetting data frames.
  \code{\link{factor}} for definition of factors.
  \code{\link{drop}} for dropping array dimensions.
  \code{\link{drop1}} for dropping terms from a model.
  \code{\link{[.factor}} for subsetting of factors.
}
\examples{
aq <- transform(airquality, Month = factor(Month, labels = month.abb[5:9]))
aq <- subset(aq, Month != "Jul")
table(aq$Month)
table(droplevels(aq)$Month)
}
\keyword{category}
\keyword{NA}
