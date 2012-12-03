#  File src/library/stats/R/reshape.R
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

reshape <-
    function(data, varying = NULL, v.names = NULL, timevar = "time",
             idvar = "id",
             ids = 1L:NROW(data), times = seq_along(varying[[1L]]),
             drop = NULL, direction, new.row.names = NULL,
             sep = ".",
             split = if (sep == "") {
                 list(regexp = "[A-Za-z][0-9]", include = TRUE)
             } else {list(regexp = sep, include = FALSE, fixed = TRUE)})
{

    if (!is.character(sep) || length(sep) != 1L)
        stop("'sep' must be a character string")

    ix2names <- function(ix)
        if (is.character(ix)) ix else names(data)[ix]

    guess <- function(nms,re = split$regexp,drop = !split$include,
                      fixed=if(is.null(split$fixed)) FALSE else split$fixed)
    {
        if (drop)
            nn <- do.call("rbind",strsplit(nms, re, fixed = fixed))
        else
            nn <- cbind(substr(nms, 1L, regexpr(re,nms)),
                        substr(nms, regexpr(re,nms) + 1L, 10000L))

        if (ncol(nn) != 2L)
            stop("failed to guess time-varying variables from their names")


        vn <- unique(nn[,1])
        v.names <- split(nms, factor(nn[, 1L], levels = vn))
        times <- unique(nn[, 2L])
        attr(v.names,"v.names") <- vn
        tt <- tryCatch(as.numeric(times), warning = function(w) times)
        attr(v.names,"times") <- tt
        v.names
    }

    reshapeLong <-
        function(data, varying, v.names = NULL, timevar, idvar,
                 ids = 1L:NROW(data), times,
                 drop = NULL,new.row.names = NULL)
        {
            ll <- unlist(lapply(varying,length))
            if (any(ll != ll[1L]))
                stop("'varying' arguments must be the same length")
            if (ll[1L] != length(times)) stop("'times' is wrong length")

            if (!is.null(drop)) {
                if (is.character(drop))
                    drop <- names(data) %in% drop
                data <- data[, if (is.logical(drop)) !drop else -drop, drop = FALSE]
            }

            ## store information for back-transformation.
            undoInfo <- list(varying = varying, v.names = v.names,
                             idvar = idvar, timevar = timevar)

            ## multiple id variables
            if (length(idvar) > 1L) {
                repeat({
                    tempidname <- basename(tempfile("tempID"))
                    if (!(tempidname %in% names(data))) break
                })
                data[ ,tempidname] <- interaction(data[, idvar], drop=TRUE)
                idvar <- tempidname
                drop.idvar <- TRUE
            } else drop.idvar <- FALSE


            d <- data
            all.varying <- unlist(varying)
            d <- d[,!(names(data) %in% all.varying), drop = FALSE]
            d[,timevar] <- times[1L]

            if (is.null(v.names))
                v.names <- unlist(lapply(varying, function(x) x[1L]))

            for(i in seq_along(v.names))
                d[, v.names[i]] <- data[, varying[[i]][1L] ]

            if (!(idvar %in% names(data))) d[, idvar] <- ids

            rval <- d

            if (length(times) == 1L) {
                if (drop.idvar) rval[, idvar] <- NULL
                return(rval)
            }
            if (is.null(new.row.names))
                row.names(rval) <- paste(d[, idvar], times[1L], sep = ".")
            else
                row.names(rval) <- new.row.names[1L:NROW(rval)]

            for(i in 2L:length(times)) {
                d[,timevar] <- times[i]
                for(j in seq_along(v.names))
                    d[, v.names[j]] <- data[, varying[[j]][i]]

                if (is.null(new.row.names))
                    row.names(d) <- paste(d[, idvar], times[i], sep = ".")
                else
                    row.names(d) <- new.row.names[NROW(rval) + 1L:NROW(d)]
                rval <- rbind(rval, d) ##inefficient. So sue me.
            }

            ## if we created a temporary id variable, drop it
            if (drop.idvar) rval[, idvar] <- NULL

            attr(rval,"reshapeLong") <- undoInfo
            return(rval)
        } ## re..Long()

    reshapeWide <- function(data,timevar,idvar,varying = NULL,v.names = NULL,
                            drop = NULL,new.row.names = NULL)
    {
        if (!is.null(drop)) {
            if (is.character(drop)) drop <- names(data) %in% drop
            data <- data[, if (is.logical(drop)) !drop else -drop, drop = FALSE]
        }
        undoInfo <- list(v.names = v.names,  timevar = timevar,idvar = idvar)

        orig.idvar <- idvar
        if (length(idvar) > 1L) {
            repeat({
                tempidname <- basename(tempfile("tempID"))
                if (!(tempidname %in% names(data))) break
            })
            data[, tempidname] <- interaction(data[, idvar], drop=TRUE)
            idvar <- tempidname
            drop.idvar <- TRUE
        } else drop.idvar <- FALSE

        ## times <- sort(unique(data[,timevar]))
        ## varying and times must have the same order
        times <- unique(data[, timevar])
        if (any(is.na(times)))
            warning("there are records with missing times, which will be dropped.")
        undoInfo$times <- times

        if (is.null(v.names))
            v.names <- names(data)[!(names(data) %in% c(timevar, idvar, orig.idvar))]

        if (is.null(varying)) varying <- outer(v.names, times, paste, sep = sep)
        if (is.list(varying)) varying <- do.call("rbind", varying)

        undoInfo$varying <- varying

        keep <- !(names(data) %in% c(timevar, v.names, idvar, orig.idvar))
        if(any(keep)) {
            rval <- data[keep]
            tmp <- data[, idvar]
            really.constant <-
                unlist(lapply(rval,
                              function(a) all(tapply(a, as.vector(tmp),
                                                     function(b) length(unique(b)) == 1L))))
            if (!all(really.constant))
                warning(gettextf("some constant variables (%s) are really varying",
                                 paste(names(rval)[!really.constant],collapse = ",")), domain = NA)
        }

        rval <- data[!duplicated(data[, idvar]),
                     !(names(data) %in% c(timevar, v.names)), drop = FALSE]

        for(i in seq_along(times)) {
            thistime <- data[data[, timevar] %in% times[i], ]
            tab <- table(thistime[, idvar])
            if (any(tab > 1L))
                warning(sprintf("multiple rows match for %s=%s: first taken",
                                timevar, times[i]), domain = NA)
            rval[, varying[, i]] <-
                thistime[match(rval[, idvar], thistime[, idvar]), v.names]
        }

        if (!is.null(new.row.names)) row.names(rval) <- new.row.names

        ## temporary id variable to be dropped.
        if (drop.idvar) rval[, idvar] <- NULL

        ## information for back-transformation
        attr(rval,"reshapeWide") <- undoInfo

        rval
    } ## re..Wide()

    ## Begin reshape()

    if (missing(direction)) {
        undo <- c("wide", "long")[c("reshapeLong", "reshapeWide")
                                  %in% names(attributes(data))]
        if (length(undo) == 1L) direction <- undo
    }
    direction <- match.arg(direction, c("wide", "long"))


    switch(direction,
           "wide" =
       {
           back <- attr(data,"reshapeLong")
           if (missing(timevar) && missing(idvar) && !is.null(back)) {
               reshapeWide(data, idvar = back$idvar, timevar = back$timevar,
                           varying = back$varying, v.names = back$v.names,
                           new.row.names = new.row.names)
           } else {
               reshapeWide(data, idvar = idvar, timevar = timevar,
                           varying = varying, v.names = v.names, drop = drop,
                           new.row.names = new.row.names)
           }

       },
           "long" =
       {
           if (missing(varying)) {
               back <- attr(data,"reshapeWide")
               if (is.null(back))
                   stop("no 'reshapeWide' attribute, must specify 'varying'")
               varying <- back$varying
               idvar <- back$idvar
               timevar <- back$timevar
               v.names <- back$v.names
               times <- back$times
           }

           if (is.matrix(varying))
               varying <- split(varying, row(varying))
           if (is.null(varying))
               stop("'varying' must be nonempty list or vector")
           if(is.atomic(varying)) {
               varying <- ix2names(varying) # normalize
               if (missing(v.names))
                   varying <- guess(varying)
               else {
                   if (length(varying) %% length(v.names))
                       stop("length of 'v.names' does not evenly divide length of 'varying'")
                   ntimes <- length(varying) %/% length(v.names)
                   if (missing(times))
                       times <- seq_len(ntimes)
                   else if (length(times) != ntimes)
                       stop("length of 'varying' must be the product of length of 'v.names' and length of 'times'")
                   varying <- split(varying, rep(v.names, ntimes))
                   attr(varying, "v.names") <- v.names
                   attr(varying, "times") <- times
               }
           }
           else varying <- lapply(varying, ix2names)

           ## This must happen after guess()
           if (missing(v.names) && !is.null(attr(varying,"v.names"))) {
               v.names <- attr(varying, "v.names")
               times <- attr(varying, "times")
           }
           reshapeLong(data, idvar = idvar, timevar = timevar,
                       varying = varying, v.names = v.names, drop = drop,
                       times = times, ids = ids, new.row.names = new.row.names)
       })
}
