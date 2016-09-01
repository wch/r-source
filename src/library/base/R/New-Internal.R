#  File src/library/base/R/New-Internal.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
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

geterrmessage <- function() .Internal(geterrmessage())

try <- function(expr, silent = FALSE,
                outFile = getOption("try.outFile", default = stderr())) {
    tryCatch(expr, error = function(e) {
        call <- conditionCall(e)
        if (! is.null(call)) {
            ## Patch up the call to produce nicer result for testing as
            ## try(stop(...)).  This will need adjusting if the
            ## implementation of tryCatch changes.
            ## Use identical() since call[[1L]] can be non-atomic.
            if (identical(call[[1L]], quote(doTryCatch)))
                call <- sys.call(-4L)
            dcall <- deparse(call)[1L]
            prefix <- paste("Error in", dcall, ": ")
            LONG <- 75L # to match value in errors.c
            msg <- conditionMessage(e)
            sm <- strsplit(msg, "\n")[[1L]]
            w <- 14L + nchar(dcall, type="w") + nchar(sm[1L], type="w")
            ## this could be NA if any of this is invalid in a MBCS
            if(is.na(w))
                w <-  14L + nchar(dcall, type="b") + nchar(sm[1L], type="b")
            if (w > LONG)
                prefix <- paste0(prefix, "\n  ")
        }
        else prefix <- "Error : "
        msg <- paste0(prefix, conditionMessage(e), "\n")
        ## Store the error message for legacy uses of try() with
        ## geterrmessage().
        .Internal(seterrmessage(msg[1L]))
        if (! silent && identical(getOption("show.error.messages"), TRUE)) {
            cat(msg, file = outFile)
            .Internal(printDeferredWarnings())
        }
        invisible(structure(msg, class = "try-error", condition = e))
    })
}

comment <- function(x) .Internal(comment(x))
`comment<-` <- function(x, value) .Internal("comment<-"(x, value))

logb <- function(x, base=exp(1)) if(missing(base)) log(x) else log(x, base)

atan2 <- function(y, x) .Internal(atan2(y, x))

beta <- function(a, b) .Internal( beta(a, b))
lbeta <- function(a, b) .Internal(lbeta(a, b))

psigamma <- function(x, deriv = 0L) .Internal(psigamma(x, deriv))

factorial <- function(x) gamma(x + 1)
lfactorial <- function(x) lgamma(x + 1)

choose <- function(n, k) .Internal(choose(n, k))
lchoose <- function(n, k) .Internal(lchoose(n, k))

##-- 2nd part --
R.Version <- function() .Internal(Version())

commandArgs <- function(trailingOnly = FALSE) {
    args <- .Internal(commandArgs())
    if(trailingOnly) {
        m <- match("--args", args, 0L)
        if(m) args[-seq_len(m)] else character()
    } else args
}

args <- function(name) .Internal(args(name))

cbind <- function(..., deparse.level = 1)
    .Internal(cbind(deparse.level, ...))

rbind <- function(..., deparse.level = 1)
    .Internal(rbind(deparse.level, ...))

## for methods:::bind_activation
.__H__.cbind <- cbind
.__H__.rbind <- rbind


# convert deparsing options to bitmapped integer

.deparseOpts <- function(control) {
    opts <- pmatch(as.character(control),
                   ## the exact order of these is determined by the integer codes in
                   ## ../../../include/Defn.h
                   c("all",
                     "keepInteger", "quoteExpressions", "showAttributes",
                     "useSource", "warnIncomplete", "delayPromises",
                     "keepNA", "S_compatible", "hexNumeric", "digits17"))
    if (anyNA(opts))
        stop(sprintf(ngettext(as.integer(sum(is.na(opts))),
                              "deparse option %s is not recognized",
                              "deparse options %s are not recognized"),
                     paste(sQuote(control[is.na(opts)]), collapse=", ")),
             call. = FALSE, domain = NA)
    if (any(opts == 1L))
        opts <- unique(c(opts[opts != 1L], 2L,3L,4L,5L,6L,8L)) # not (7,9:11)
    if(10L %in% opts && 11L %in% opts)
        stop('"hexNumeric" and "digits17" are mutually exclusive')
    return(sum(2^(opts-2)))
}

deparse <-
    function(expr, width.cutoff = 60L,
	     backtick = mode(expr) %in% c("call", "expression", "(", "function"),
	     control = c("keepInteger", "showAttributes", "keepNA"),
             nlines = -1L)
    .Internal(deparse(expr, width.cutoff, backtick,
                      .deparseOpts(control), nlines))

do.call <- function(what, args, quote = FALSE, envir = parent.frame())
{
    if (!is.list(args))
	stop("second argument must be a list")
    if (quote)
	args <- lapply(args, enquote)
    .Internal(do.call(what, args, envir))
}

drop <- function(x) .Internal(drop(x))

format.info <- function(x, digits = NULL, nsmall = 0L)
    .Internal(format.info(x, digits, nsmall))

gc <- function(verbose = getOption("verbose"),	reset=FALSE)
{
    res <- .Internal(gc(verbose, reset))
    res <- matrix(res, 2L, 7L,
		  dimnames = list(c("Ncells","Vcells"),
		  c("used", "(Mb)", "gc trigger", "(Mb)",
		    "limit (Mb)", "max used", "(Mb)")))
    if(all(is.na(res[, 5L]))) res[, -5L] else res
}
gcinfo <- function(verbose) .Internal(gcinfo(verbose))
gctorture <- function(on = TRUE) .Internal(gctorture(on))
gctorture2 <- function(step, wait = step, inhibit_release = FALSE)
    .Internal(gctorture2(step, wait, inhibit_release))

is.unsorted <- function(x, na.rm = FALSE, strictly = FALSE)
{
    if(length(x) <= 1L) return(FALSE)
    if(!na.rm && anyNA(x))
	return(NA)
    ## else
    if(na.rm && any(ii <- is.na(x)))
	x <- x[!ii]
    .Internal(is.unsorted(x, strictly))
}

nchar <- function(x, type = "chars", allowNA = FALSE, keepNA = NA)
    .Internal(nchar(x, type, allowNA, keepNA))

polyroot <- function(z) .Internal(polyroot(z))

readline <- function(prompt = "") .Internal(readline(prompt))
search <- function() .Internal(search())
searchpaths <- function()
{
    s <- search()
    paths <-
        lapply(seq_along(s), function(i) attr(as.environment(i), "path"))
    paths[[length(s)]] <- system.file()
    m <- grep("^package:", s)
    if(length(m)) paths[-m] <- as.list(s[-m])
    unlist(paths)
}

sprintf <- function(fmt, ...) .Internal(sprintf(fmt, ...))

##-- DANGER ! ---   substitute(list(...))  inside functions !!!
##substitute <- function(expr, env=baseenv()) .Internal(substitute(expr, env))

t.default <- function(x) .Internal(t.default(x))
typeof <- function(x) .Internal(typeof(x))


memory.profile <- function() .Internal(memory.profile())

capabilities <- function(what = NULL)
{
    z  <- .Internal(capabilities())
    if(!is.null(what))
        z <- z[match(what, names(z), 0L)]
    if(.Platform$OS.type == "windows") return(z)
    ## Now we need to deal with any NA entries if X11 is unknown.
    nas <- names(z[is.na(z)])
    if(any(nas %in% c("X11", "jpeg", "png", "tiff"))) {
        ## This might throw an X11 error
         z[nas] <- tryCatch(.Internal(capabilitiesX11()),
                            error = function(e) FALSE)
    }
    z
}

inherits <- function(x, what, which = FALSE)
	.Internal(inherits(x, what, which))

NextMethod <- function(generic=NULL, object=NULL, ...)
    .Internal(NextMethod(generic, object,...))

data.class <- function(x) {
    if (length(cl <- oldClass(x)))
	cl[1L]
    else {
	l <- length(dim(x))
        if (l == 2L) "matrix" else if(l) "array" else mode(x)
    }
}

encodeString <- function(x, width = 0L, quote = "", na.encode = TRUE,
                         justify = c("left", "right", "centre", "none"))
{
    at <- attributes(x)
    x <- as.character(x) # we want e.g. NULL to work
    attributes(x) <- at  # preserve names, dim etc
    oldClass(x) <- NULL  # but not class
    justify <- match(match.arg(justify),
                     c("left", "right", "centre", "none")) - 1L
    .Internal(encodeString(x, width, quote, justify, na.encode))
}

l10n_info <- function() .Internal(l10n_info())

iconv <- function(x, from = "", to = "", sub = NA, mark = TRUE, toRaw = FALSE)
{
    if(! (is.character(x) || (is.list(x) && is.null(oldClass(x)))))
        x <- as.character(x)
    .Internal(iconv(x, from, to, as.character(sub), mark, toRaw))
}

iconvlist <- function()
{
    int <- .Internal(iconv(NULL, "", "", "", TRUE, FALSE))
    if(length(int)) return(sort.int(int))
    icfile <- system.file("iconvlist", package="utils")
    if(!nchar(icfile, type="bytes"))
        stop("'iconvlist' is not available on this system")
    ext <- readLines(icfile)
    if(!length(ext)) stop("'iconvlist' is not available on this system")
    ## glibc has lines ending //, some versions with a header and some without.
    ## libiconv has lines with multiple entries separated by spaces
    cnt <- grep("//$", ext)
    if(length(cnt)/length(ext) > 0.5) {
        ext <- grep("//$", ext, value = TRUE)
        ext <- sub("//$", "", ext)
    }
    sort.int(unlist(strsplit(ext, "[[:space:]]")))
}

Cstack_info <- function() .Internal(Cstack_info())

reg.finalizer <- function(e, f, onexit = FALSE)
    .Internal(reg.finalizer(e, f, onexit))

Encoding <- function(x) .Internal(Encoding(x))
`Encoding<-` <- function(x, value) .Internal(setEncoding(x, value))

setTimeLimit <- function(cpu = Inf, elapsed = Inf, transient = FALSE)
    .Internal(setTimeLimit(cpu, elapsed, transient))
setSessionTimeLimit <- function(cpu = Inf, elapsed = Inf)
    .Internal(setSessionTimeLimit(cpu, elapsed))

icuSetCollate <- function(...) .Internal(icuSetCollate(...))
icuGetCollate <- function(type = c("actual", "valid")) {
    type <- match.arg(type)
    .Internal(icuGetCollate(match(type, c("actual", "valid"))))
}

extSoftVersion <- function() .Internal(eSoftVersion())

libcurlVersion <- function() .Internal(curlVersion())

curlGetHeaders <- function(url, redirect = TRUE, verify = TRUE)
    .Internal(curlGetHeaders(url, redirect, verify))


lengths <- function(x, use.names=TRUE) .Internal(lengths(x, use.names))


## base has no S4 generics
.noGenerics <- TRUE
