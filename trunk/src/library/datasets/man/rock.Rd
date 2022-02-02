% File src/library/datasets/man/rock.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1994-8 W. N. Venables and B. D. Ripley
% Distributed under GPL 2 or later

% Manually converted to Rd data file description format by KH.
\name{rock}
\docType{data}
\alias{rock}
\title{Measurements on Petroleum Rock Samples}
\description{
    Measurements on 48 rock samples from a petroleum reservoir.}
\usage{rock}
\format{
    A data frame with 48 rows and 4 numeric columns.

    \tabular{rll}{
        [,1] \tab area  \tab area of pores space, in pixels
        out of 256 by 256 \cr
        [,2] \tab peri  \tab perimeter in pixels \cr
        [,3] \tab shape \tab perimeter/sqrt(area) \cr
        [,4] \tab perm  \tab permeability in milli-Darcies
    }
}
\details{
    Twelve core samples from petroleum reservoirs were sampled by 4
    cross-sections.  Each core sample was measured for permeability, and
    each cross-section has total area of pores, total perimeter of
    pores, and shape.
}
\source{
    Data from BP Research, image analysis by Ronit Katz, U. Oxford.
}
\keyword{datasets}
