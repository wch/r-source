#  File src/library/methods/R/substituteDirect.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

substituteDirect <-
  ## subsitute the for the variables named in the second argument the corresponding
  ## objects, substituting into `object'.
  ##
  ## This function differs from the ordinary `substitute' in that it treats its first argument
  ## in the standard S way, by evaluating it.  In contrast, `substitute' does
  ## not evaluate its first argument.
  function(object, frame = parent.frame(), cleanFunction = TRUE)
{
    value <- .Call(C_do_substitute_direct, object, frame)
     if(cleanFunction && is.function(value)) {
       ## unset any local environment
       environment(value) <- .GlobalEnv
     }
    value
  }

