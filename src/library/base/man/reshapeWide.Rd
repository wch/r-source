\name{reshapeWide}
\alias{reshapeWide}
\title{Reshape data frame to wide format}
\description{
  (EXPERIMENTAL). Convert data frame with repeated measurements in long
  format, 1 row per observation to wide format with repeated
  observations in multiple variables across rows.
}
\usage{
reshapeWide(x, i=reshape.i, j=reshape.j,
            val=reshape.v, jnames=levels(j))
}
\arguments{
  \item{x}{The data frame to convert}
  \item{i}{Factor or numeric. Indicates observations in same row of the
    wide format}
  \item{j}{Factor or numeric. Indicates observations in same column of
    wide format}
  \item{val}{Value to reshape for.}
  \item{jnames}{Names of new variables in reshaped frame}
}
\details{
  \code{val}, \code{i}, and \code{j} are interpreted relative to
  \code{x}.
  This essentially places the values of \code{val} in a table defined by
  \code{i} and \code{j}, which are assumed to span the data set (exactly
  one observation in each cell of their cross-tabulation). Any
  other variables in the data frame will be
  assumed to have the same value for all values of \code{j} (given
  \code{i}) and are
  collapsed into a single value.
}
\value{
   The reshaped data frame.
}
\note{The defaults are set to use the names that result
  from a call to \code{reshapeLong}.}

\section{WARNING}{There is no check that the names given to the new
  columns are
  valid identifiers or unique within the data frame. Variables in
  \code{x} are not checked to have constant values given \code{i}.}
\section{Bugs}{There ought to be a way to have multiple \code{val} variables.}

\seealso{\code{\link{reshapeLong}}, \code{\link{unstack}}}

\examples{
dd<-as.data.frame(matrix(1:24,4))
(dd1<-reshapeLong(dd,V3:V5))
reshapeWide(dd1)
reshapeWide(dd1,jnames=c("A","B","C"))
}
\keyword{ manip }%-- one or more ...
