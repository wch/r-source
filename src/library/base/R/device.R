dev.list <-
    function()
{
    if(exists(".Devices")) {
	n <- get(".Devices")
    }
    else {
	n <- list("null device")
    }
    n <- unlist(n)
    i <- seq(along = n)[n != ""]
    names(i) <- n[i]
    i <- i[-1]
    if(length(i) == 0)
	return(NULL)
    else i
}

dev.cur <-
    function()
{
    if(!exists(".Devices")) {
	.Devices <- list("null device")
    }
    num.device <- .Internal(dev.cur())
    names(num.device) <- .Devices[[num.device]]
    num.device
}

dev.set <-
    function(which = dev.next())
{
    which <- .Internal(dev.set(as.integer(which)))
    if(exists(".Devices")) {
	assign(".Device", get(".Devices")[[which]])
    }
    else {
	.Devices <- list("null device")
    }
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
    if(exists(".Devices")) {
	.Devices <- get(".Devices")
    }
    else {
	.Devices <- list("null device")
    }
    .Devices[[which]] <- ""
    assign(".Devices", .Devices)
    .Internal(dev.off(as.integer(which)))
    assign(".Device", .Devices[[dev.cur()]])
    dev.cur()
}

dev.copy <- function(device, ..., which = dev.next())
{
    if(!missing(which) & !missing(device))
	stop("Cannot supply which and device at the same time.")
    old.device <- dev.cur()
    if(old.device == 1)
	stop("Cannot copy the null device.")
    if(missing(device)) {
	if(which == 1)
	    stop("Cannot copy to the null device.")
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
    dev.off(dev.copy(device = device, ...)) # user must still print this
    dev.set(current.device)
}

dev.control <- function(displaylist)
{
    if(!missing(displaylist)) {
	if(displaylist == "inhibit")
	    .Internal(dev.control())
	else stop(paste("displaylist should be inhibit"))
    }
    invisible()
}

graphics.off <- function ()
{
    while ((which <- dev.cur()) != 1)
	dev.off(which)
}
