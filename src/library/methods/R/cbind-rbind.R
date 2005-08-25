
## Experimental {similar c() code: MM's ~/R/MM/Pkg-ex/methods/cbind-etc.R}
cbind <- function(..., deparse.level = 1)
{
    if(deparse.level != 1) .NotYetUsed("deparse.level != 1")
    na <- nargs()
    if(na <= 1) return(.Internal(cbind(...)))
    ## else :
    sc <- sys.call()
    r <- cbind2(..1, if(na == 2) ..2 else do.call(cbind, list(...)[-1]))

    ## All the rest: Setting colnames correctly [needs some diligence!]
    Ncol <- function(x) {
        d <- dim(x); if(length(d) == 2) d[2] else as.integer(length(x) > 0) }
    setN <- function(len, i0, nams)
        if(len) colnames(r)[(i0+1):(i0+len)] <<-
            if(len > 1 || is.null(nams)) rep.int("", len) else nams

    if((l1 <- Ncol(..1)) && is.null(colnames(..1)))
        setN(l1, 0, names(sc[2]))
    if(na == 2 && (l2 <- Ncol(..2)) && is.null(colnames(..2)))
        setN(l2, l1, names(sc[3]))
    r
}

## Possibly, this cbind() must somehow "replace"  cbind() in "base"
## One way that *FAILS* is in the .on[un]Load() functions in  ./zzz.R


### cbind2 () :  Generic and methods need to be "method-bootstrapped"
### --------   --> ./MethodsListClass.R
