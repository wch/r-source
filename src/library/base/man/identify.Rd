\name{identify}
\title{Identify Points in a Scatter Plot}
\usage{
identify(x, \dots)
identify.default(x, y = NULL, labels = seq(along = x), pos = FALSE,
	 n = length(x), plot = TRUE, offset = 0.5, \dots)
}
\alias{identify}
\alias{identify.default}
\arguments{
  \item{x,y}{coordinates of points in a scatter plot.  Alternatively, any
    object which defines coordinates (a plotting structure, time
    series etc.) can be given as \code{x} and \code{y} left undefined.}
  \item{labels}{an optional vector, the same length as \code{x} and
    \code{y}, giving labels for the points.}
  \item{pos}{if \code{pos} is \code{TRUE}, a component is added to the
    return value which indicates where text was plotted relative to each
    identified point (1=below, 2=left, 3=above and 4=right).}
  \item{n}{the maximum number of points to be identified.}
  \item{plot}{if \code{plot} is \code{TRUE}, the labels are printed at
    the points and if \code{FALSE} they are omitted.}
  \item{offset}{the distance (in character widths) which separates the
    label from identified points.}
  \item{\dots}{further arguments to \code{par(.)}.}
}
\description{
  \code{identify} reads the position of the graphics pointer when the
  (first) mouse button is pressed.  It then searches the coordinates
  given in \code{x} and \code{y} for the point closest to the pointer.
  If this point is close to the pointer, its index will be returned as
  part of the value of the call.
}
\details{
  If in addition, \code{plot} is \code{TRUE}, the point is labelled with
  the corresponding element of \code{text}.

  The labels are placed either below, to the left, above or to the right
  of the identified point, depending on where the cursor was.

  The identification process is terminated by pressing any mouse button
  other than the first, or by clicking outside the graphics window.
}
\value{
  If \code{pos} is \code{FALSE}, an integer
  vector containing the indexes of the identified points.

  If \code{pos} is \code{TRUE}, a list containing a component
  \code{ind}, indicating which points were identified and a component
  \code{pos}, indicating where the labels were placed relative to the
  identified points.
}
\keyword{iplot}
