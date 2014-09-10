#  File src/library/utils/R/edit.R
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

check_for_XQuartz <- function()
{
    r_arch <- .Platform$r_arch
    DSO <- file.path(R.home("modules"), "R_de.so")
    out <- system2("otool", c("-L", shQuote(DSO)), stdout = TRUE)
    ind <- grep("libX11[.][0-9]+[.]dylib", out)
    if(length(ind)) {
        this <- sub(" .*", "", sub("^\t", "", out[ind]))
        if(!file.exists(this))
            stop("X11 library is missing: install XQuartz from xquartz.macosforge.org", domain = NA)
    }
}

dataentry <- function (data, modes)
{
    check <- Sys.getenv("_R_CHECK_SCREEN_DEVICE_", "")
    msg <- "dataentry() should not be used in examples etc"
    if (identical(check, "stop"))
        stop(msg, domain = NA)
    else if (identical(check, "warn"))
        warning(msg, immediate. = TRUE, noBreaks. = TRUE, domain = NA)

    if(!is.list(data) || !length(data) || !all(sapply(data, is.vector)))
        stop("invalid 'data' argument")
    if(!is.list(modes) ||
       (length(modes) && !all(sapply(modes, is.character))))
        stop("invalid 'modes' argument")
    if (grepl("darwin", R.version$os)) check_for_XQuartz()
    .External2(C_dataentry, data, modes)
}

View <- function (x, title)
{
    check <- Sys.getenv("_R_CHECK_SCREEN_DEVICE_", "")
    msg <- "View() should not be used in examples etc"
    if (identical(check, "stop"))
        stop(msg, domain = NA)
    else if (identical(check, "warn"))
        warning(msg, immediate. = TRUE, noBreaks. = TRUE, domain = NA)

    ## could multi-line deparse with maliciously-designed inputs
    if(missing(title)) title <- paste("Data:", deparse(substitute(x))[1])
    as.num.or.char <- function(x)
    {
        if (is.character(x)) x
        else if (is.numeric(x)) {storage.mode(x) <- "double"; x}
        else as.character(x)
    }
    x0 <- as.data.frame(x)
    x <- lapply(x0, as.num.or.char)
    rn <- row.names(x0)
    if(any(rn != seq_along(rn))) x <- c(list(row.names = rn), x)
    if(!is.list(x) || !length(x) || !all(sapply(x, is.atomic)) ||
       !max(sapply(x, length)))
        stop("invalid 'x' argument")
    if (grepl("darwin", R.version$os)) check_for_XQuartz()
    invisible(.External2(C_dataviewer, x, title))
}

edit <- function(name,...)UseMethod("edit")

edit.default <-
    function (name = NULL, file = "", title = NULL,
              editor = getOption("editor"), ...)
{
    if (is.null(title)) title <- deparse(substitute(name))
    if (is.function(editor)) invisible(editor(name = name, file = file, title = title))
    else .External2(C_edit, name, file, title, editor)
}

edit.data.frame <-
    function(name, factor.mode = c("character", "numeric"),
             edit.row.names =  any(row.names(name) != 1L:nrow(name)), ...)
{
    if (.Platform$OS.type == "unix"  && .Platform$GUI != "AQUA")
        if(.Platform$GUI == "unknown" || Sys.getenv("DISPLAY") == "" )
            return (edit.default(name, ...))

    is.vector.unclass <- function(x) is.vector(unclass(x))
    if (length(name) && !all(sapply(name, is.vector.unclass)
                                 | sapply(name, is.factor)))
        stop("can only handle vector and factor elements")

    if (grepl("darwin", R.version$os)) check_for_XQuartz()

    factor.mode <- match.arg(factor.mode)

    as.num.or.char <- function(x)
    {
        if (is.numeric(x)) x
        else if (is.factor(x) && factor.mode == "numeric") as.numeric(x)
        else as.character(x)
    }

    attrlist <- lapply(name, attributes)
    datalist <- lapply(name, as.num.or.char)
    factors <- if (length(name))
        which(sapply(name, is.factor))
    else
        numeric()

    logicals <- if (length(name))
    	which(sapply(name, is.logical))
    else
    	numeric()

    if(length(name)) {
        has_class <-
            sapply(name, function(x) (is.object(x) || isS4(x)) && !is.factor(x))
        if(any(has_class))
            warning(sprintf(ngettext(sum(has_class),
                                    "class discarded from column %s",
                                    "classes discarded from columns %s"),
                            paste(sQuote(names(name)[has_class]),
                                  collapse=", ")),
                    domain = NA, call. = FALSE, immediate. = TRUE)
    }

    modes <- lapply(datalist, mode)
    if (edit.row.names) {
        datalist <- c(list(row.names = row.names(name)), datalist)
        modes <- c(list(row.names = "character"), modes)
    }
    rn <- attr(name, "row.names")

    out <- .External2(C_dataentry, datalist, modes)
    if(length(out) == 0L) {
        ## e.g. started with 0-col data frame or NULL, and created no cols
        return (name)
    }
    lengths <- sapply(out, length)
    maxlength <- max(lengths)
    if (edit.row.names) rn <- out[[1L]]
    for (i in which(lengths != maxlength))
         out[[i]] <- c(out[[i]], rep.int(NA, maxlength - lengths[i]))
    if (edit.row.names) {
        out <- out[-1L]
        if((ln <- length(rn)) < maxlength)
            rn <- c(rn, paste0("row", (ln+1):maxlength))
    } else if(length(rn) != maxlength) rn <- seq_len(maxlength)
    for (i in factors) {
        if(factor.mode != mode(out[[i]])) next # user might have switched mode
        a <- attrlist[[i]]
        if (factor.mode == "numeric") {
            o <- as.integer(out[[i]])
            ok <- is.na(o) | (o > 0 & o <= length(a$levels))
            if (any(!ok)) {
                warning(gettextf("invalid factor levels in '%s'", names(out)[i]),
                        domain = NA)
                o[!ok] <- NA
            }
	    attributes(o) <- a
        } else {
            o <- out[[i]]
            if (any(new <- is.na(match(o, c(a$levels, NA_integer_))))) {
                new <- unique(o[new])
                warning(gettextf("added factor levels in '%s'", names(out)[i]),
                        domain = NA)
                o <- factor(o, levels=c(a$levels, new),
                            ordered = is.ordered(o))
            } else {
                o <- match(o, a$levels)
                attributes(o) <- a
            }
        }
        out[[i]] <- o
    }
    for (i in logicals) out[[i]] <- as.logical(out[[i]])

    attr(out, "row.names") <- rn
    attr(out, "class") <- "data.frame"
    if (edit.row.names) {
        if(anyDuplicated(rn)) {
            warning("edited row names contain duplicates and will be ignored")
            attr(out, "row.names") <- seq_len(maxlength)
        }
    }
    out
}

edit.matrix <-
    function(name, edit.row.names = !is.null(dn[[1L]]), ...)
{
    if (.Platform$OS.type == "unix" && .Platform$GUI != "AQUA")
        if(.Platform$GUI == "unknown" || Sys.getenv("DISPLAY")=="" )
            return (edit.default(name, ...))
    if(!is.matrix(name) ||
       ! mode(name) %in% c("numeric", "character", "logical") ||
       any(dim(name) < 1))
        stop("invalid input matrix")

    if (grepl("darwin", R.version$os)) check_for_XQuartz()

    ## logical matrices will be edited as character
    logicals <- is.logical(name)
    if (logicals) mode(name) <- "character"
    if(is.object(name) || isS4(name))
        warning("class of 'name' will be discarded",
                call. = FALSE, immediate. = TRUE)

    dn <- dimnames(name)
    datalist <- split(name, col(name))
    if(!is.null(dn[[2L]])) names(datalist) <- dn[[2L]]
    else names(datalist) <- paste0("col", 1L:ncol(name))
    modes <- as.list(rep.int(mode(name), ncol(name)))
    ## guard aginst user error (PR#10500)
    if(edit.row.names && is.null(dn[[1L]]))
        stop("cannot edit NULL row names")
    if (edit.row.names) {
        datalist <- c(list(row.names = dn[[1L]]), datalist)
        modes <- c(list(row.names = "character"), modes)
    }

    out <- .External2(C_dataentry, datalist, modes)

    lengths <- sapply(out, length)
    maxlength <- max(lengths)
    if (edit.row.names) rn <- out[[1L]]
    for (i in which(lengths != maxlength))
         out[[i]] <- c(out[[i]], rep.int(NA, maxlength - lengths[i]))
    if (edit.row.names) {
        out <- out[-1L]
        if((ln <- length(rn)) < maxlength)
            rn <- c(rn, paste0("row", (ln+1L):maxlength))
    }
    out <- do.call("cbind", out)
    if (edit.row.names)
        rownames(out) <- rn
    else if(!is.null(dn[[1L]]) && length(dn[[1L]]) == maxlength)
        rownames(out) <- dn[[1L]]
    if (logicals) mode(out) <- "logical"
    out
}

file.edit <-
  function (..., title = file, editor=getOption("editor"), fileEncoding="")
{
    file <- path.expand(c(...))
    title <- rep(as.character(title), len=length(file))
    if(nzchar(fileEncoding) && fileEncoding != "native.enc") {
        tfile <- file
        for(i in seq_along(file)) {
            ## We won't know when that is done with
            ## so leave around for the R session.
            tfile <- tempfile()
            con <- file(file[i], encoding = fileEncoding)
            writeLines(readLines(con), tfile)
            close(con)
            file[i] <- tfile
        }
    }
    if (is.function(editor)) invisible(editor(file = file, title = title))
    else invisible(.External2(C_fileedit, file, title, editor))
}

vi <- function(name = NULL, file = "")
    edit.default(name, file, editor = "vi")

emacs <- function(name = NULL, file = "")
    edit.default(name, file, editor = "emacs")

xemacs <- function(name = NULL, file = "")
    edit.default(name, file, editor = "xemacs")

xedit <- function(name = NULL, file = "")
    edit.default(name, file, editor = "xedit")

pico <- function(name = NULL, file = "")
    edit.default(name, file, editor = "pico")

