dataentry <- function (data, modes) {
    if(!is.list(data) || !length(data) || !all(sapply(data, is.vector)))
        stop("invalid data argument")
    if(!is.list(modes) ||
       (length(modes) && !all(sapply(modes, is.character))))
        stop("invalid modes argument")
    .Internal(dataentry(data, modes))
}

edit <- function(name,...)UseMethod("edit")

edit.default <-
    function (name = NULL, file = "", title = NULL, editor = getOption("editor"), ...)
{
    if(is.matrix(name) &&
       (mode(name) == "numeric" || mode(name) == "character"))
        edit.matrix(name=name, ...)
    else {
	if (is.null(title)) title <- deparse(substitute(name))
	.Internal(edit(name, file, title, editor))
    }
}

edit.data.frame <-
    function(name, factor.mode = c("character", "numeric"),
             edit.row.names =  any(row.names(name) != 1:nrow(name)), ...)
{
    if (.Platform$OS.type == "unix"  && .Platform$GUI != "AQUA")
        if(.Platform$GUI == "unknown" || Sys.getenv("DISPLAY")=="" )
            return (edit.default(name, ...))

    is.vector.unclass <- function(x) is.vector(unclass(x))
    if (length(name) > 0 && !all(sapply(name, is.vector.unclass)
                                 | sapply(name, is.factor)))
        stop("can only handle vector and factor elements")

    factor.mode <- match.arg(factor.mode)

    as.num.or.char <- function(x)
    {
        ## Would as.character be a better default?  BDR 2000/5/3
        if (is.character(x)) x
        else if (is.logical(x) || (is.factor(x) && factor.mode == "character")) as.character(x)
        else as.numeric(x)
    }

    attrlist <- lapply(name, attributes)
    datalist <- lapply(name, as.num.or.char)
    factors <- if (length(name) > 0)
        which(sapply(name, is.factor))
    else
        numeric(0)

    logicals <- if (length(name) > 0)
    	which(sapply(name, is.logical))
    else
    	numeric(0)

    modes <- lapply(datalist, mode)
    if (edit.row.names) {
        datalist <- c(list(row.names=row.names(name)), datalist)
        modes <- c(list(row.names="character"), modes)
    }
    out <- .Internal(dataentry(datalist, modes))
    lengths <- sapply(out, length)
    maxlength <- max(lengths)
    if (edit.row.names) rn <- out[[1]]
    for (i in which(lengths != maxlength))
         out[[i]] <- c(out[[i]], rep.int(NA, maxlength - lengths[i]))
    if (edit.row.names) {
        out <- out[-1]
        if((ln <- length(rn)) < maxlength)
            rn <- c(rn, paste("row", (ln+1):maxlength, sep=""))
    }
    for (i in factors) {
        if(mode(out[[i]]) == "numeric") next # user might have switched mode
        a <- attrlist[[i]]
        if (factor.mode == "numeric") {
            o <- as.integer(out[[i]])
            ok <- is.na(o) | (o > 0 & o <= length(a$levels))
            if (any(!ok)) {
                warning(paste("invalid factor levels in", names(out)[i]))
                o[!ok] <- NA
            }
	    attributes(o) <- a
        } else {
            o <- out[[i]]
            if (any(new <- is.na(match(o, c(a$levels, NA))))) {
                new <- unique(o[new])
                warning(paste("added factor levels in", names(out)[i]))
                o <- factor(o, levels=c(a$levels, new), ordered=is.ordered(o))
            } else {
                o <- match(o, a$levels)
                attributes(o) <- a
            }
        }
        out[[i]] <- o
    }
    for (i in logicals) out[[i]] <- as.logical(out[[i]])

    out <- as.data.frame(out) # will convert cols switched to char into factors
    if (edit.row.names) {
        if(any(duplicated(rn)))
            warning("edited row names contain duplicates and will be ignored")
        else row.names(out) <- rn
    }
    out
}

edit.matrix <-
    function(name, edit.row.names = any(rownames(name) != 1:nrow(name)), ...)
{
    if (.Platform$OS.type == "unix" && .Platform$GUI != "AQUA")
        if(.Platform$GUI == "unknown" || Sys.getenv("DISPLAY")=="" )
            return (edit.default(name, ...))
    if(!is.matrix(name) ||
       !(mode(name) == "numeric" || mode(name) == "character" || mode(name) == "logical")
       || any(dim(name) < 1))
        stop("invalid input matrix")
    logicals <- is.logical(name)
    if (logicals) mode(name) <- "character"
    dn <- dimnames(name)
    if(is.null(dn[[1]])) edit.row.names <- FALSE
    datalist <- split(name, col(name))
    if(!is.null(dn[[2]])) names(datalist) <- dn[[2]]
    else names(datalist) <- paste("col", 1:ncol(name), sep = "")
    modes <- as.list(rep.int(mode(name), ncol(name)))
    if (edit.row.names) {
        datalist <- c(list(row.names=dn[[1]]), datalist)
        modes <- c(list(row.names="character"), modes)
    }
    out <- .Internal(dataentry(datalist, modes))
    lengths <- sapply(out, length)
    maxlength <- max(lengths)
    if (edit.row.names) rn <- out[[1]]
    for (i in which(lengths != maxlength))
         out[[i]] <- c(out[[i]], rep.int(NA, maxlength - lengths[i]))
    if (edit.row.names) {
        out <- out[-1]
        if((ln <- length(rn)) < maxlength)
            rn <- c(rn, paste("row", (ln+1):maxlength, sep=""))
    }
    out <- do.call("cbind", out)
    if (edit.row.names) rownames(out) <- rn
    else if(!is.null(dn[[1]]))  rownames(out) <- dn[[1]]
    if(!is.null(dn[[2]]))  colnames(out) <- dn[[2]]
    if (logicals) mode(out) <- "logical"
    out
}

file.edit <-
  function (..., title = file, editor=getOption("editor"))
{
    file <- c(...)
    .Internal(file.edit(file, rep(as.character(title), len=length(file)), editor))
}

vi <- function(name=NULL, file="")
    edit.default(name, file, editor="vi")

emacs <- function(name=NULL, file="")
    edit.default(name, file, editor="emacs")

xemacs <- function(name=NULL, file="")
    edit.default(name, file, editor="xemacs")

xedit <- function(name=NULL, file="")
    edit.default(name, file, editor="xedit")

pico <- function(name=NULL, file="")
    edit.default(name, file, editor="pico")

