#  File src/library/grid/R/origin.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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

valid.origin <- function(origin) {
  origin <- as.integer(match(origin,
                             c("bottom.left", "top.left",
                               "bottom.right", "top.right")) - 1)
  if (any(is.na(origin)))
    stop("invalid 'origin'")
  origin
}

origin.left <- function(origin) {
  switch (origin,
          bottom.left = TRUE,
          bottom.right = FALSE,
          top.left = TRUE,
          top.right = FALSE)
}

origin.right <- function(origin) {
  switch (origin,
          bottom.left = FALSE,
          bottom.right = TRUE,
          top.left = FALSE,
          top.right = TRUE)
}

origin.bottom <- function(origin) {
  switch (origin,
          bottom.left = TRUE,
          bottom.right = TRUE,
          top.left = FALSE,
          top.right = FALSE)
}

origin.top <- function(origin) {
  switch (origin,
          bottom.left = FALSE,
          bottom.right = FALSE,
          top.left = TRUE,
          top.right = TRUE)
}

swap.origin.horizontal <- function(origin) {
  switch (origin,
          bottom.left = "bottom.right",
          bottom.right = "bottom.left",
          top.left = "top.right",
          top.right = "top.left")
}

swap.origin.vertical <- function(origin) {
  switch (origin,
          bottom.left = "top.left",
          bottom.right = "top.right",
          top.left = "bottom.left",
          top.right = "bottom.right")
}
