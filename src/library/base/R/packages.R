## A simple S3 class for package versions, and associated methods.

## We represent "vectors" of package versions as lists of sequences of
## integers, as obtained by splitting by splitting the package version
## strings on the separators.  By default, only valid version specs
## (sequences of integers of length at least two, corresponding to major
## and minor, separated by '.' or '-'), are allowed.  If strictness is
## turned off, invalid specs result in integer() (rather than NA) to
## keep things simple.  (Note: using NULL would make subscripting more
## cumbersome ...)

## (In fact, the underlying mechanism could easily be extended to more
## version specs.  E.g., one could allow "letters" in version numbers by
## replacing the non-sep characters in the version string by their ASCII
## codes.)

package_version <-
function(x, strict = TRUE)
{
    x <- as.character(x)
    y <- rep.int(list(integer()), length(x))
    valid_package_version_regexp <-
        sprintf("^%s$", .standard_regexps()$valid_package_version)
    if(length(x) > 0) {
        ok <- (regexpr(valid_package_version_regexp, x) > -1)
        if(!all(ok) && strict) stop("invalid version spec")
        y[ok] <- lapply(strsplit(x[ok], "[.-]"), as.integer)
    }
    class(y) <- "package_version"
    y
}

is.package_version <-
function(x)
    inherits(x, "package_version")
as.package_version <-
function(x)
    if(is.package_version(x)) x else package_version(x)

.encode_package_version <-
function(x, base = NULL)
{
    if(!is.package_version(x)) stop("wrong class")
    if(is.null(base)) base <- max(unlist(x), 0) + 1
    lens <- as.numeric(sapply(x, length))
    ## We store the lengths so that we know when to stop when decoding.
    ## Alternatively, we need to be smart about trailing zeroes.  One
    ## approach is to increment all numbers in the version specs and
    ## base by 1, and when decoding only retain the non-zero entries and
    ## decrement by 1 one again.
    x <- as.numeric(sapply(x,
                           function(t)
                           sum(t / base^seq(0, length = length(t)))))
    attr(x, "base") <- base
    attr(x, "lens") <- lens
    x
}
.decode_package_version <-
function(x, base = NULL)
{
    if(is.null(base)) base <- attr(x, "base")
    if(!is.numeric(base)) stop("wrong arg")
    lens <- attr(x, "lens")
    y <- vector("list", length = length(x))
    for(i in seq(along = x)) {
        n <- lens[i]
        encoded <- x[i]
        decoded <- integer(n)
        for(k in seq(length = n)) {
            decoded[k] <- encoded %/% 1
            encoded <- base * (encoded %% 1)
        }
        y[[i]] <- as.integer(decoded)
    }
    class(y) <- "package_version"
    y
}

as.character.package_version <-
function(x)
    as.character(unlist(lapply(x, paste, collapse = ".")))
print.package_version <-
function(x, ...)
{
    print(noquote(sQuote(as.character(x))), ...)
    invisible(x)
}
Ops.package_version <-
function(e1, e2)
{
    if(nargs() == 1)
        stop("unary ", .Generic, " not defined for package_version objects")
    boolean <- switch(.Generic, "<" = , ">" = , "==" = , "!=" = ,
        "<=" = , ">=" = TRUE, FALSE)
    if(!boolean)
        stop(.Generic, " not defined for package_version objects")
    if(!is.package_version(e1)) e1 <- as.package_version(e1)
    if(!is.package_version(e2)) e2 <- as.package_version(e2)
    base <- max(unlist(e1), unlist(e2), 0) + 1
    e1 <- .encode_package_version(e1, base = base)
    e2 <- .encode_package_version(e2, base = base)
    NextMethod(.Generic)
}
Summary.package_version <-
function(x, ...)
{
    ok <- switch(.Generic, max = , min = TRUE, FALSE)
    if(!ok)
        stop(.Generic, " not defined for package_version objects")
    x <- list(x, ...)
    x$na.rm <- NULL
    x <- do.call("c", lapply(x, as.package_version))
    ## </FIXME>
    switch(.Generic,
           max = x[which.max(.encode_package_version(x))],
           min = x[which.min(.encode_package_version(x))])
}

c.package_version <-
function(..., recursive = FALSE)
{
    x <- unlist(lapply(list(...), as.package_version),
                recursive = FALSE)
    class(x) <- "package_version"
    x
}

"[.package_version" <-
function(x, i, j)
{
    y <- if(missing(j))
        unclass(x)[i]
    else
        lapply(unclass(x)[i], "[", j)
    ## Change sequences which are NULL or contains NAs to integer().
    bad <- as.logical(sapply(y,
                             function(t) is.null(t) || any(is.na(t))))
    if(any(bad))
        y[bad] <- rep.int(list(integer()), length(bad))
    class(y) <- "package_version"
    y
}

"[[.package_version" <-
function(x, i)
    unclass(x)[[i]]

"$.package_version" <-
function(x, name)
{
    name <- pmatch(name, c("major", "minor", "patchlevel"))
    switch(name,
           major = as.integer(sapply(x, "[", 1)),
           minor = as.integer(sapply(x, "[", 2)),
           patchlevel = {
               as.integer(sapply(x,
                                 function(s) s[min(3, length(s))]))
           })
}

as.data.frame.package_version <- as.data.frame.vector

getRversion <- function()
    package_version(paste(R.version[c("major", "minor")], collapse = "."))
