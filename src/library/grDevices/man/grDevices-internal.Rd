% File src/library/grDevices/man/grDevices-internal.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2023 R Core Team
% Distributed under GPL 2 or later

\name{grDevices-internal}
\alias{.clipPath}
\alias{.defineGroup}
\alias{.devUp}
\alias{.linearGradientPattern}
\alias{.mask}
\alias{.opIndex}
\alias{.radialGradientPattern}
\alias{.ruleIndex}
\alias{.setClipPath}
\alias{.setMask}
\alias{.setPattern}
\alias{.tilingPattern}
\alias{.useGroup}
\title{Internal \pkg{grDevices} Functions}
\description{
  Internal \pkg{grDevices} functions
}
\usage{
.clipPath(fun, rule) 
.defineGroup(source, op, destination) 
.devUp() 
.linearGradientPattern(colours = c("black", "white"),
                       stops = seq(0, 1, length.out = length(colours)), 
                       x1 = 0, y1 = 0, x2 = 1, y2 = 1, extend = "pad") 
.mask(fun, type) 
.opIndex(x) 
.radialGradientPattern(colours = c("black", "white"),
                       stops = seq(0, 1, length.out = length(colours)), 
                       cx1 = 0, cy1 = 0, r1 = 0,
                       cx2 = 1, cy2 = 1, r2 = 0.5,
                       extend = "pad") 
.ruleIndex(x) 
.setClipPath(path, index) 
.setMask(mask, ref) 
.setPattern(pattern) 
.tilingPattern(fun, x, y, width, height, extend) 
.useGroup(ref, trans) 
}
\details{
  These are used for communication between the graphics engine and
  graphics systems, \pkg{graphics} and \pkg{grid}, and are
  not to be called by the user.
}
\keyword{ internal }
