#  File src/library/grDevices/R/device.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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


.known_interactive.devices <-
    c("X11", "X11cairo", "quartz", "windows", "JavaGD", "CairoWin", "CairoX11")

dev.interactive <- function(orNone = FALSE)
{
    if(!interactive()) return(FALSE)
    if(.Device %in% .known_interactive.devices) return(TRUE)
    if(!(orNone && .Device == "null device")) return(FALSE)
    ## at this point we have mo active device.
    newdev <- getOption("device")
    if(is.character(newdev)) newdev %in% .known_interactive.devices
    else { # a function
        if(.Platform$OS.type == "windows") identical(newdev, windows)
        else identical(newdev, X11) || identical(newdev, quartz)
    }
}

deviceIsInteractive <- function(name = NULL)
{
    if(length(name)) {
        if(!is.character(name)) stop("'name' must be a character vector")
        unlockBinding(".known_interactive.devices", asNamespace("grDevices"))
        .known_interactive.devices <<- c(.known_interactive.devices, name)
        lockBinding(".known_interactive.devices", asNamespace("grDevices"))
        invisible(.known_interactive.devices)
    } else .known_interactive.devices
}


dev.list <- function()
{
    n <- if(exists(".Devices")) get(".Devices") else list("null device")
    n <- unlist(n)
    i <- seq_along(n)[n != ""]
    names(i) <- n[i]
    i <- i[-1L]
    if(length(i) == 0L) NULL else i
}

dev.cur <- function()
{
    if(!exists(".Devices"))
	.Devices <- list("null device")
    num.device <- .External(C_devcur)
    names(num.device) <- .Devices[[num.device]]
    num.device
}

dev.set <-
    function(which = dev.next())
{
    which <- .External(C_devset, as.integer(which))
    names(which) <- .Devices[[which]]
    which
}

dev.next <-
    function(which = dev.cur())
{
    if(!exists(".Devices"))
	.Devices <- list("null.device")
    num.device <- .External(C_devnext, as.integer(which))
    names(num.device) <- .Devices[[num.device]]
    num.device
}

dev.prev <-
    function(which = dev.cur())
{
    if(!exists(".Devices"))
	.Devices <- list("null device")
    num.device <- .External(C_devprev, as.integer(which))
    names(num.device) <- .Devices[[num.device]]
    num.device
}

dev.off <-
    function(which = dev.cur())
{
    if(which == 1)
	stop("cannot shut down device 1 (the null device)")
    .External(C_devoff, as.integer(which))
    dev.cur()
}

dev.copy <- function(device, ..., which = dev.next())
{
    if(!missing(which) & !missing(device))
	stop("cannot supply 'which' and 'device' at the same time")
    old.device <- dev.cur()
    if(old.device == 1)
	stop("cannot copy from the null device")
    if(missing(device)) {
	if(which == 1)
	    stop("cannot copy to the null device")
	else if(which == dev.cur())
	    stop("cannot copy device to itself")
	dev.set(which)
    }
    else {
	if(!is.function(device))
	    stop("'device' should be a function")
	else device(...)
    }
    ## protect against failure
    on.exit(dev.set(old.device))
    .External(C_devcopy, old.device)
    on.exit()
    dev.cur()
}

dev.print <- function(device = postscript, ...)
{
    current.device <- dev.cur()
    nm <- names(current.device)[1L]
    if(nm == "null device") stop("no device to print from")
    if(!dev.displaylist())
        stop("can only print from a screen device")
    oc <- match.call()
    oc[[1L]] <- quote(grDevices::dev.copy)
    oc$device <- device
    din <- graphics::par("din"); w <- din[1L]; h <- din[2L]
    if(missing(device)) { ## safe way to recognize postscript
        if(is.null(oc$file)) oc$file <- ""
        hz0 <- oc$horizontal
        hz <- if(is.null(hz0)) ps.options()$horizontal else eval.parent(hz0)
        paper <- oc$paper
        if(is.null(paper)) paper <- ps.options()$paper
        if(paper == "default") paper <- getOption("papersize")
        paper <- tolower(paper)
        switch(paper,
               a4 = 	 {wp <- 8.27; hp <- 11.69},
               legal =	 {wp <- 8.5;  hp <- 14.0},
               executive={wp <- 7.25; hp <- 10.5},
               { wp <- 8.5; hp <- 11}) ## default is "letter"

        wp <- wp - 0.5; hp <- hp - 0.5  # allow 0.25" margin on each side.
        if(!hz && is.null(hz0) && h < wp && wp < w && w < hp) {
            ## fits landscape but not portrait
            hz <- TRUE
        } else if (hz && is.null(hz0) && w < wp && wp < h && h < hp) {
            ## fits portrait but not landscape
            hz <- FALSE
        } else {
            h0 <- ifelse(hz, wp, hp)
            if(h > h0) { w <- w * h0/h; h <- h0 }
            w0 <- ifelse(hz, hp, wp)
            if(w > w0) { h <- h * w0/w; w <- w0 }
        }
        if(is.null(oc$pointsize)) {
            pt <- ps.options()$pointsize
            oc$pointsize <- pt * w/din[1L]
        }
        if(is.null(hz0)) oc$horizontal <- hz
        if(is.null(oc$width)) oc$width <- w
        if(is.null(oc$height)) oc$height <- h
    } else {
        devname <- deparse(substitute(device))
        if(devname %in% c("png", "jpeg", "bmp") &&
           is.null(oc$width) && is.null(oc$height))
            warning("need to specify one of 'width' and 'height'")
        if(is.null(oc$width))
            oc$width <- if(!is.null(oc$height)) w/h * eval.parent(oc$height) else w
        if(is.null(oc$height))
            oc$height <- if(!is.null(oc$width)) h/w * eval.parent(oc$width) else h
    }
    ## protect against failure (PR#9801)
    on.exit(dev.set(current.device))
    dev.off(eval.parent(oc))
}

dev.copy2eps <- function(...)
{
    current.device <- dev.cur()
    nm <- names(current.device)[1L]
    if(nm == "null device") stop("no device to print from")
    if(!dev.displaylist())
        stop("can only print from a screen device")
    oc <- match.call()
    oc[[1L]] <- quote(grDevices::dev.copy)
    oc$device <- postscript
    oc$onefile <- FALSE
    oc$horizontal <- FALSE
    if(is.null(oc$paper))
        oc$paper <- "special"
    din <- dev.size("in"); w <- din[1L]; h <- din[2L]
    if(is.null(oc$width))
        oc$width <- if(!is.null(oc$height)) w/h * eval.parent(oc$height) else w
    if(is.null(oc$height))
        oc$height <- if(!is.null(oc$width)) h/w * eval.parent(oc$width) else h
    if(is.null(oc$file)) oc$file <- "Rplot.eps"
    ## protect against failure (PR#9801)
    on.exit(dev.set(current.device))
    dev.off(eval.parent(oc))
}

dev.copy2pdf <- function(..., out.type = "pdf")
{
    out.type <- match.arg(out.type, c("pdf", "quartz", "cairo"))
    current.device <- dev.cur()
    nm <- names(current.device)[1L]
    if(nm == "null device") stop("no device to print from")
    if(!dev.displaylist())
        stop("can only print from a screen device")
    oc <- match.call()
    oc[[1L]] <- quote(grDevices::dev.copy)
    if(out.type == "quartz" && capabilities("aqua")) {
        oc$device <- quartz
        oc$type <- "pdf"
    } else if(out.type == "cairo" && capabilities("cairo")) {
        oc$device <- cairo_pdf
        oc$onefile <- FALSE # future-proofing
    } else {
        oc$device <- pdf
        ## the defaults in pdf() are all customizable, so we override
        ## even those which are the ultimate defaults.
        oc$onefile <- FALSE
        if(is.null(oc$paper)) oc$paper <- "special"
    }
    oc$out.type <- NULL
    din <- dev.size("in"); w <- din[1L]; h <- din[2L]
    if(is.null(oc$width))
        oc$width <- if(!is.null(oc$height)) w/h * eval.parent(oc$height) else w
    if(is.null(oc$height))
        oc$height <- if(!is.null(oc$width)) h/w * eval.parent(oc$width) else h
    if(is.null(oc$file)) oc$file <- "Rplot.pdf"
    ## protect against failure (PR#9801)
    on.exit(dev.set(current.device))
    dev.off(eval.parent(oc))
}

dev.control <- function(displaylist = c("inhibit", "enable"))
{
    if(dev.cur() <= 1)
        stop("dev.control() called without an open graphics device")
    if(!missing(displaylist)) {
        displaylist <- match.arg(displaylist)
	.External(C_devcontrol, displaylist == "enable")
    } else stop("argument is missing with no default")
    invisible()
}

dev.displaylist <- function()
{
    if(dev.cur() <= 1)
        stop("dev.displaylist() called without an open graphics device")
    .External(C_devdisplaylist)
}

## This records graphics ops and manipulates visibility, so needs to stay .Internal
recordGraphics <- function(expr, list, env)
  .Internal(recordGraphics(substitute(expr), list, env))

graphics.off <- function ()
{
    while ((which <- dev.cur()) != 1) dev.off(which)
    invisible()
}

dev.new <- function(..., noRStudioGD = FALSE)
{
    dev <- getOption("device")
    if(!is.character(dev) && !is.function(dev))
        stop("invalid setting for 'getOption(\"device\")'")
    if(noRStudioGD && is.character(dev) && dev == "RStudioGD")
        dev <- .select_device()
    if(is.character(dev)) {
        ## this is documented to be searched for from workspace,
        ## then in the grDevices namespace.
        ## We could restrict the search to functions, but the C
        ## code in devices.c does not.
        dev <- if(exists(dev, .GlobalEnv)) get(dev, .GlobalEnv)
        else if(exists(dev, asNamespace("grDevices")))
            get(dev, asNamespace("grDevices"))
        else stop(gettextf("device '%s' not found", dev), domain=NA)
    }
    ## only include named args in the devices's arglist
    a <- list(...)
    a2 <- names(formals(dev))
    a <- a[names(a) %in% a2]
    if(identical(dev, pdf)) {
        ## Take care not to open device on top of another.
        if(is.null(a[["file"]]) && file.exists("Rplots.pdf")) {
            fe <- file.exists(tmp <- paste0("Rplots", 1L:999, ".pdf"))
            if(all(fe)) stop("no suitable unused file name for pdf()")
            message(gettextf("dev.new(): using pdf(file=\"%s\")", tmp[!fe][1L]),
                    domain=NA)
            a$file <- tmp[!fe][1L]
        }
    } else if(identical(dev, postscript)) {
        ## Take care not to open device on top of another.
        if(is.null(a[["file"]]) && file.exists("Rplots.ps")) {
            fe <- file.exists(tmp <- paste0("Rplots", 1L:999, ".ps"))
            if(all(fe)) stop("no suitable unused file name for postscript()")
            message(gettextf("dev.new(): using postscript(file=\"%s\")",
                             tmp[!fe][1L]), domain=NA)
            a$file <- tmp[!fe][1L]
        }
    } else if (!is.null(a[["width"]]) && !is.null(a[["height"]]) &&
               (identical(dev, png) || identical(dev, jpeg) ||
                identical(dev, bmp) || identical(dev, tiff))) {
        ## some people want dev.new(width=12, height=7) to be portable
        if(is.null(a[["units"]]) && is.null(a[["res"]])) {
            a$units <- "in"
            a$res <- 72
        }
    }
    do.call(dev, a)
}

### Check for a single valid integer format
checkIntFormat <- function(s)
{
    ## OK if no unescaped %, so first remove those
    s <- gsub("%%", "", s)
    if(length(grep("%", s)) == 0L) return(TRUE)
    ## now remove at most one valid(ish) integer format
    s <- sub("%[#0 ,+-]*[0-9.]*[diouxX]", "", s)
    length(grep("%", s)) == 0L
}

devAskNewPage <- function(ask=NULL) .External2(C_devAskNewPage, ask)

dev.size <- function(units = c("in", "cm", "px"))
{
    units <- match.arg(units)
    size <- .External(C_devsize)
    if(units == "px") size else size * graphics::par("cin")/graphics::par("cra") *
        if(units == "cm") 2.54 else 1
}

dev.hold <- function(level = 1L) .External(C_devholdflush, max(0L, level))
dev.flush <- function(level = 1L) .External(C_devholdflush, -max(0L, level))

dev.capture <- function(native = FALSE) .External(C_devcapture, native)

dev.capabilities <- function(what = NULL)
{
    zz <- .External(C_devcap)
    z <- vector("list", 6L)
    names(z) <-  c("semiTransparency", "transparentBackground",
                   "rasterImage", "capture", "locator",
                   "events")
    z[[1L]] <- c(NA, FALSE, TRUE)[zz[1L] + 1L]
    z[[2L]] <- c(NA, "no", "fully", "semi")[zz[2L] + 1L]
    z[[3L]] <- c(NA, "no", "yes", "non-missing")[zz[3L] + 1L]
    z[[4L]] <- c(NA, FALSE, TRUE)[zz[4L] + 1L]
    z[[5L]] <- c(NA, FALSE, TRUE)[zz[5L] + 1L]
    z[[6L]] <- c( "",
                  if (zz[6L]) "MouseDown",
                  if (zz[7L]) "MouseMove",
                  if (zz[8L]) "MouseUp",
                  if (zz[9L]) "Keybd" )[-1L]
    if (!is.null(what)) z[charmatch(what, names(z), 0L)] else z
}

## for use in dev.new and .onLoad
.select_device <- function() {
    ## Use device functions rather than names to make it harder to get masked.
    if(!nzchar(defdev <- Sys.getenv("R_DEFAULT_DEVICE"))) defdev <- pdf
    if(interactive()) {
        if(nzchar(intdev <- Sys.getenv("R_INTERACTIVE_DEVICE"))) intdev
        else {
            if(.Platform$OS.type == "windows") windows
            else {
                dsp <- Sys.getenv("DISPLAY")
                ## the first condition is running under R.app.
                ## Recentish OS X with XQuartz installed sets DISPLAY on startup
                ## and what this is set to changed ca Yosemite
                ## It would be better to grep for org.macosforge.xquartz .
                if(.Platform$GUI == "AQUA" ||
                    ((!nzchar(dsp) || grepl("^/tmp/launch-|^/private/tmp/com.apple.launchd", dsp))
                     && .Call(C_makeQuartzDefault))) quartz
                else if(nzchar(dsp) && .Platform$GUI %in% c("X11", "Tk")) X11
                else defdev
            }
        }
    } else defdev
}
