edit <-
    function(name,...)UseMethod("edit")

edit.default<-
    function (name = NULL, file = "", editor = getOption("editor"))
    .Internal(edit(name, file, editor))

edit.data.frame<-
    function(name,factor.mode=c("numeric","character"),...)
{
    if (getenv("DISPLAY")=="" || .Platform$OS.type == "windows")
        return (edit.default(name,...))

    if (!all(sapply(name, is.vector) | sapply(name, is.factor)))
        stop("Can only handle vector and factor elements")

    factor.mode <- match.arg(factor.mode)

    as.num.or.char<-function(x)
    {
        if (is.character(x))
            x
        else if (is.factor(x) && factor.mode == "character")
            as.character(x)
        else
            as.numeric(x)
    }

    attrlist <- lapply(name, attributes)
    datalist <- lapply(name, as.num.or.char)
    factors <- which(sapply(name, is.factor))
    modes <- lapply(datalist, mode)
    out <- .Internal(dataentry(datalist, modes))
    lengths <- sapply(out, length)
    maxlength <- max(lengths)
    for (i in which(lengths != maxlength))
         out[[i]] <- c(out[[i]], rep(NA, maxlength-lengths[i]))
    for (i in factors) {
        a <- attrlist[[i]]
        if (factor.mode == "numeric") {
            o <- as.integer(out[[i]])
            ok <- is.na(o) | o > 0 & o <= length(a$levels)
            if (any(!ok)) {
                warning(paste("invalid factor levels in",
                               names(out)[i]))
                o[o <= 0] <- NA
                o[o > length(a$levels)] <- NA
            }
        } else {
            o <- out[[i]]
            if (any(is.na(match(o, c(a$levels, NA)))))
                warning(paste("invalid factor levels in",
                               names(out)[i]))
            o <- match(o, a$levels)
        }
        attributes(o) <- a
        out[[i]] <- o
    }
    as.data.frame(out)
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

