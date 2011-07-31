#  File src/library/parallel/R/RngStream.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

nextRNGStream <- function(seed)
{
    if(!is.integer(seed) || seed[1L] %% 100L != 7L)
        stop("invalid value of 'seed'")
    .Call("nextStream", seed, PACKAGE = "parallel")
}

nextRNGSubStream <- function(seed)
{
    if(!is.integer(seed) || seed[1L] %% 100L != 7L)
        stop("invalid value of 'seed'")
    .Call("nextSubStream", seed, PACKAGE = "parallel")
}
