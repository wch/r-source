% File src/library/grDevices/man/grDevices-defunct.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2025 R Core Team
% Distributed under GPL 2 or later

\name{grDevices-defunct}
\title{Defunct Objects in Package \pkg{grDevices}}
\alias{grDevices-defunct}
%------ PLEASE: put \alias{.} here for *each* (and keep them) !
\alias{xfig}

\description{
  The functions or variables listed here are no longer part of \R as
  they are no longer supported.
}
\usage{
# Defunct in R 4.5.0
xfig(file = if(onefile) "Rplots.fig" else "Rplot\%03d.fig",
     onefile = FALSE, encoding = "none",
     paper = "default", horizontal = TRUE,
     width = 0, height = 0, family = "Helvetica",
     pointsize = 12, bg = "transparent", fg = "black",
     pagecentre = TRUE, defaultfont = FALSE, textspecial = FALSE)
}
\seealso{
  \code{\link{Deprecated}},
  \code{\link{Defunct}}
}
\keyword{internal}
