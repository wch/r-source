% File src/library/stats/man/stats-deprecated.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{stats-deprecated}
\alias{stats-deprecated}
\alias{plclust}
%------ NOTE:  ../R/stats-deprecated.R   must be synchronized with this!
\title{Deprecated Functions in Package \pkg{stats}}
%------ PLEASE: one \alias{.} for EACH ! (+ one \usage{} & \arguments{} for all)
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as the next release.
}
\usage{
plclust(tree, hang = 0.1, unit = FALSE, level = FALSE, hmin = 0,
        square = TRUE, labels = NULL, plot. = TRUE,
        axes = TRUE, frame.plot = FALSE, ann = TRUE,
        main = "", sub = NULL, xlab = NULL, ylab = "Height")
}
\arguments{
  \item{tree}{an object of the type produced by \code{\link{hclust}}.}

  \item{hang}{The fraction of the plot height by which labels should hang
    below the rest of the plot.
    A negative value will cause the labels to hang down from 0.}

  \item{unit}{logical.  If true, the splits are plotted at
    equally-spaced heights rather than at the height in the object.}
  
  \item{labels}{A character vector of labels for the leaves of the
    tree. By default the row names or row numbers of the original data are
    used. If \code{labels = FALSE} no labels at all are plotted.}

  \item{axes, frame.plot, ann}{logical flags as in \code{\link{plot.default}}.}
  
  \item{main, sub, xlab, ylab}{character strings for
    \code{\link{title}}.  \code{sub} and \code{xlab} have a non-NULL
    default when there's a \code{tree$call}.}
  \item{\dots}{Further graphical arguments.  E.g., \code{cex} controls
    the size of the labels (if plotted) in the same way as \code{\link{text}}.}

  \item{hmin}{numeric.  All heights less than \code{hmin} are regarded
    as being \code{hmin}: this can be used to suppress detail at the
    bottom of the tree.}
  
  \item{level, square, plot.}{unimplemented arguments for S-PLUS compatibility.}
}

\details{
  %-- Either:
  %There are currently no deprecated functions in this package.
  %-- Or:
  %% explain *why* it's deprecated, and \code{\link{..}} to new
  \code{plcust} is a deprecated wrapper for the \code{plot} method for
  \code{\link{hclust}}, provided long ago for S-PLUS compatibility.
}
\seealso{
  \code{\link{Deprecated}}
}
\keyword{misc}
