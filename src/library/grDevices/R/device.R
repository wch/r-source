dev.interactive <- function()
  interactive() &&
  .Device %in% c("X11", "GTK", "gnome", "quartz", "windows", "JavaGD")

dev.list <- function()
{
    n <- if(exists(".Devices")) get(".Devices") else list("null device")
    n <- unlist(n)
    i <- seq(along = n)[n != ""]
    names(i) <- n[i]
    i <- i[-1]
    if(length(i) == 0) NULL else i
}

dev.cur <- function()
{
    if(!exists(".Devices"))
	.Devices <- list("null device")
    num.device <- .Internal(dev.cur())
    names(num.device) <- .Devices[[num.device]]
    num.device
}

dev.set <-
    function(which = dev.next())
{
    which <- .Internal(dev.set(as.integer(which)))
#     if(exists(".Devices")) {
# 	assign(".Device", get(".Devices")[[which]])
#     }
#     else {
# 	.Devices <- list("null device")
#     }
    names(which) <- .Devices[[which]]
    which
}

dev.next <-
    function(which = dev.cur())
{
    if(!exists(".Devices"))
	.Devices <- list("null.device")
    num.device <- .Internal(dev.next(as.integer(which)))
    names(num.device) <- .Devices[[num.device]]
    num.device
}

dev.prev <-
    function(which = dev.cur())
{
    if(!exists(".Devices"))
	.Devices <- list("null device")
    num.device <- .Internal(dev.prev(as.integer(which)))
    names(num.device) <- .Devices[[num.device]]
    num.device
}

dev.off <-
    function(which = dev.cur())
{
    if(which == 1)
	stop("Cannot shut down device 1 (the null device)")
    .Internal(dev.off(as.integer(which)))
    dev.cur()
}

dev.copy <- function(device, ..., which = dev.next())
{
    if(!missing(which) & !missing(device))
	stop("Cannot supply which and device at the same time")
    old.device <- dev.cur()
    if(old.device == 1)
	stop("Cannot copy the null device")
    if(missing(device)) {
	if(which == 1)
	    stop("Cannot copy to the null device")
	else if(which == dev.cur())
	    stop("Cannot copy device to itself")
	dev.set(which)
    }
    else {
	if(!is.function(device))
	    stop("Argument 'device' should be a function")
	else device(...)
    }
    .Internal(dev.copy(old.device))
    dev.cur()
}

dev.print <- function(device = postscript, ...)
{
    current.device <- dev.cur()
    nm <- names(current.device)[1]
    if(nm == "null device") stop("no device to print from")
    if(!(nm %in% c("X11", "GTK", "gnome", "windows","quartz")))
        stop("can only print from screen device")
    oc <- match.call()
    oc[[1]] <- as.name("dev.copy")
    oc$device <- device
    din <- par("din"); w <- din[1]; h <- din[2]
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
            oc$pointsize <- pt * w/din[1]
        }
        if(is.null(hz0)) oc$horizontal <- hz
        if(is.null(oc$width)) oc$width <- w
        if(is.null(oc$height)) oc$height <- h
    } else {
        if(is.null(oc$width))
            oc$width <- if(!is.null(oc$height)) w/h * eval.parent(oc$height) else w
        if(is.null(oc$height))
            oc$height <- if(!is.null(oc$width)) h/w * eval.parent(oc$width) else h
    }
    dev.off(eval.parent(oc))
    dev.set(current.device)
}

dev.copy2eps <- function(...)
{
    current.device <- dev.cur()
    nm <- names(current.device)[1]
    if(nm == "null device") stop("no device to print from")
    if(!(nm %in% c("X11", "GTK", "gnome", "windows","quartz")))
        stop("can only print from screen device")
    oc <- match.call()
    oc[[1]] <- as.name("dev.copy")
    oc$device <- postscript
    oc$onefile <- FALSE
    oc$horizontal <- FALSE
    if(is.null(oc$paper))
        oc$paper <- "special"
    din <- par("din"); w <- din[1]; h <- din[2]
    if(is.null(oc$width))
        oc$width <- if(!is.null(oc$height)) w/h * eval.parent(oc$height) else w
    if(is.null(oc$height))
        oc$height <- if(!is.null(oc$width)) h/w * eval.parent(oc$width) else h
    if(is.null(oc$file)) oc$file <- "Rplot.eps"
    dev.off(eval.parent(oc))
    dev.set(current.device)
}

dev.control <- function(displaylist = c("inhibit", "enable"))
{
    if(dev.cur() <= 1)
        stop("dev.control() called without an open graphics device")
    if(!missing(displaylist)) {
        displaylist <- match.arg(displaylist)
	.Internal(dev.control(displaylist == "enable"))
    } else stop("argument is missing with no default")
    invisible()
}

recordGraphics <- function(expr, list, env) {
  .Internal(recordGraphics(substitute(expr), list, env))
}

graphics.off <- function ()
{
    while ((which <- dev.cur()) != 1)
	dev.off(which)
}
