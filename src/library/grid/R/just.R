# NOTE: the order of the strings in these conversion functions must
# match the order of the enums in ../src/lattice.h
# NOTE: the result of match() is an integer, but subtracting 1 converts
# to real => have to convert back to integer for passing to C code

# If the user specifies two values, the first is horizontal
# justification and the second is vertical

# If the user specifies only one value, use the following
# conversion table to give a second default value
#
# bottom  -->  centre, bottom
# left    -->  left,   centre
# right   -->  right,  centre
# top     -->  centre, top
# centre  -->  centre, centre

 valid.just <- function(just, n=2) {
   if (length(just) < n)
     just <- rep(just, length.out=n)
   just <- as.integer(match(just, c("left", "right", "bottom", "top",
                                    "centre", "center")) - 1)
   if (any(is.na(just)))
     stop("Invalid justification")
   just
 }

valid.just <- function(just) {
  if (length(just) == 1) {
    # single value may be any valid just
    just <- as.integer(match(just[1], c("left", "right", "bottom", "top",
                                           "centre", "center")) - 1)
    if (any(is.na(just)))
      stop("Invalid justification")
  } else if (length(just) > 1) {
    # first value must be one of "left", "right", "centre", or "center"
    just[1] <- as.integer(match(just[1], c("left", "right", "bottom", "top",
                                           "centre", "center")) - 1)
    if (!(just[1] %in% c(0, 1, 4, 5)))
      stop("Invalid horizontal justification")
    # second value must be one of "bottom", "top", "centre", or "center"
    just[2] <- as.integer(match(just[2], c("left", "right", "bottom", "top",
                                           "centre", "center")) - 1)
    if (!(just[2] %in% c(2, 3, 4, 5)))
      stop("Invalid vertical justification")
  }
  # Extend to length 2 if necessary
  if (length(just) < 2) {
    if (length(just) == 0)
      just <- c(4, 4)
    else
      just <- switch (just[1] + 1,
                      c(0, 4), # left
                      c(1, 4), # right
                      c(4, 2), # bottom
                      c(4, 3), # top
                      c(4, 4), # centre
                      c(4, 4)) # center
  }
  as.integer(just)
}

