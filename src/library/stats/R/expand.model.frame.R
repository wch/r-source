#  File src/library/stats/R/expand.model.frame.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

expand.model.frame <- function(model, extras,
                               envir=environment(formula(model)),
                               na.expand=FALSE)
{
    ## don't use model$call$formula -- it might be a variable name
    f <- formula(model)
    data <- eval(model$call$data, envir)

    # new formula (there must be a better way...)
    ff <- foo ~ bar + baz
    if (is.call(extras))
        gg <- extras
    else
        gg <- parse(text=paste("~", paste(extras, collapse="+")))[[1L]]
    ff[[2L]] <- f[[2L]]
    ff[[3L]][[2L]] <- f[[3L]]
    ff[[3L]][[3L]] <- gg[[2L]]

    if (!na.expand){
        naa <- model$call$na.action
        subset <- model$call$subset
        rval <- eval(call("model.frame",ff, data = data, subset = subset,
                      na.action = naa),envir )
    } else {
        subset <- model$call$subset
        rval <- eval(call("model.frame",ff, data = data, subset = subset,
                          na.action = I), envir)
        oldmf <- model.frame(model)
        keep <- match(rownames(oldmf), rownames(rval))
        rval <- rval[keep, ]
        class(rval) <- "data.frame" # drop "AsIs"
    }

    return(rval)
}
