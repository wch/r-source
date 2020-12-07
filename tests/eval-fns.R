### Checking parse(* deparse()) "inversion property" ----------------------------
## EPD := eval-parse-deparse :  eval(text = parse(deparse(*)))
## Hopefully typically the identity():
pd0 <- function(expr, backtick = TRUE, ...)
    parse(text = deparse(expr, backtick=backtick, ...))
id_epd <- function(expr, control = "all", ...)
    eval(pd0(expr, control=control, ...))
dPut <- function(x, control = "all") dput(x, control=control)
##' Does 'x' contain "real" numbers
##' with > 3 digits after "." where deparse may be platform dependent?
hasReal <- function(x) {
    if(is.double(x) || is.complex(x))
        !all((x == round(x, 3)) | is.na(x))
    else if(is.logical(x) || is.integer(x) ||
	    is.symbol(x) || is.call(x) || is.environment(x) || is.character(x))
	FALSE
    else if(is.recursive(x)) # recurse :
	any(vapply(x, hasReal, NA))
    else if(isS4(x)) {
	if(length(sn <- slotNames(x)))
	    any(vapply(sn, function(s) hasReal(slot(x, s)), NA))
	else # no slots
	    FALSE # ?
    }
    else FALSE
}
isMissObj <- function(obj) identical(obj, alist(a=)[[1]])
##' Does 'obj' contain "the missing object" ?
##' @note defined recursively!
hasMissObj <- function(obj) {
    if(is.recursive(obj)) {
        if(is.function(obj) || is.language(obj))
            FALSE
        else # incl pairlist()s
            any(vapply(obj, hasMissObj, NA))
    } else isMissObj(obj)
}
check_EPD <- function(obj, show = !hasReal(obj), oNam = deparse(substitute(obj)),
                      ## FIXME: add  "niceNames" here:   ?!?
                      control = c("keepInteger","showAttributes","keepNA"),
                      not.identical.ldouble = if(!interactive()) c("t1", "t2", "ydata"),
                      eq.tol = if(noLdbl) 2*.Machine$double.eps else 0) {
    stopifnot(is.character(oNam))
    if(show) dPut(obj)
    if(is.environment(obj) || hasMissObj(obj)) {
        cat("__ not parse()able __:",
           if(is.environment(obj)) "environment" else "hasMissObj(.) is true", "\n")
        return(invisible(obj)) # cannot parse it
    }
    ob2 <- id_epd(obj)
    po <- tryCatch(pd0(obj, control=control),# the default deparse() *should* typically parse
                   error = function(e) {
                       cat("default parse(*, deparse(obj)) failed:\n  ",
                           conditionMessage(e),
                           "\n  but deparse(*, control='all') should work.\n")
                       pd0(obj, control = "all") })
    noLdbl <- (.Machine$sizeof.longdouble <= 8) ## TRUE typically from  --disable-long-double
    if(!identical(obj, ob2, ignore.environment=TRUE,
                  ignore.bytecode=TRUE, ignore.srcref=TRUE)) {
        ae <- all.equal(obj, ob2, tolerance = eq.tol, # in case of functions:
                        check.environment=FALSE)
        if(is.na(match(oNam, not.identical.ldouble))) {
            ae.txt <- "all.equal(*,*, tol = ..)"
            ## differs for "no-ldouble": sprintf("all.equal(*,*, tol = %.3g)", eq.tol)
            cat("not identical(*, ignore.env=T),", if(isTRUE(ae)) paste("but", ae.txt), "\n")
        }
        if(!isTRUE(ae)) stop("Not equal: ", ae.txt,
                             paste(c(" giving", head(ae, 2),
                                     if(length(ae) > 2) "...."), collapse = "\n  "))
    }
    if(!is.language(obj)) {
	ob2. <- eval(obj) ## almost always *NOT* identical to obj, but eval()ed
    }
    if(show || !is.list(obj)) { ## check it works when wrapped (but do not recurse inf.!)
        cat(" --> checking list(*): ")
        check_EPD(list(.chk = obj), show = FALSE, oNam=oNam, eq.tol=eq.tol)
        cat("Ok\n")
    }
    invisible(obj)
}


##' Check deparse <--> parse  consistency for *all* objects:
runEPD_checks <- function(env = .GlobalEnv) {
    stopifnot(is.environment(env))
    for(nm in ls(envir=env)) {
	cat(nm,": ", sep="")
	x <- env[[nm]]
	## if(!any(nm == "mf")) ## 'mf' [bug in deparse(mf, control="all") now fixed]
	check_EPD(x, oNam=nm)
	if(is.function(x) && !inherits(x, "classGeneratorFunction")) {
	    ## FIXME? classGeneratorFunction, e.g., mForm don't "work" yet
	    cat("checking body(.):\n")
	    check_EPD(if(is.language(bx <- body(x))) removeSource(bx) else bx)
	    cat("checking formals(.):\n"); check_EPD(formals(x))
	}
	cat("--=--=--=--=--\n")
    }
}
