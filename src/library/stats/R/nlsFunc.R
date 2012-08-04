#  File src/library/stats/R/nlsFunc.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1997,1999 Jose C. Pinheiro and  Douglas M. Bates
#            (C) 1999 Saikat DebRoy
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

###
###            Utility functions used with nls
###

###
### asOneSidedFormula is extracted from the NLME-3.1 library for S
###

asOneSidedFormula <-
  ## Converts an expression or a name or a character string
  ## to a one-sided formula
  function(object)
{
    if ((mode(object) == "call") && (object[[1L]] == "~")) {
        object <- eval(object)
    }
    if (inherits(object, "formula")) {
        if (length(object) != 2L) {
            stop(gettextf("formula '%s' must be of the form '~expr'",
                          deparse(as.vector(object))), domain = NA)
        }
        return(object)
    }
    do.call("~",
            list(switch(mode(object),
                        name = ,
                        numeric = ,
                        call = object,
                        character = as.name(object),
                        expression = object[[1L]],
                        stop(gettextf("'%s' cannot be of mode '%s'",
                                      substitute(object), mode(object)),
                             domain = NA)
                        ))
            )
}

## "FIXME": move to 'base' and make .Internal or even .Primitive
setNames <- function(object = nm, nm)
{
    names(object) <- nm
    object
}
