adist <-
function(x, y = NULL, cost = NULL, counts = FALSE, fixed = TRUE,
         partial = !fixed, ignore.case = FALSE, useBytes = FALSE)
{
    bytesToInt <- function(x) {
        if(is.na(x)) return(NA_integer_)
        as.integer(charToRaw(x))
    }

    nmx <- names(x)
    x <- as.character(x)
    names(x) <- nmx

    if(!is.null(y)) {
        nmy <- names(y)
        y <- as.character(y)
        names(y) <- nmy
    }

    if(!identical(fixed, FALSE) && !identical(partial, TRUE)) {
        ex <- Encoding(x)
        useBytes <- identical(useBytes, TRUE) || any(ex == "bytes")
        if(!is.null(y)) {
            ey <- Encoding(y)
            useBytes <- useBytes || any(ey == "bytes")
        }
        if(useBytes) {
            x <- lapply(x, bytesToInt)
            y <- if(is.null(y)) {
                x
            } else {
                lapply(y, bytesToInt)
            }
        } else {
            ignore.case <- identical(ignore.case, TRUE)
            x <- if(ignore.case) {
                lapply(tolower(enc2utf8(x)), utf8ToInt)
            } else {
                lapply(enc2utf8(x), utf8ToInt)
            }
            y <- if(is.null(y)) {
                x
            } else if(ignore.case) {
                lapply(tolower(enc2utf8(y)), utf8ToInt)
            } else {
                lapply(enc2utf8(y), utf8ToInt)
            }
        }
    }
    else if(is.null(y)) {
        y <- x
    }

    all_costs <-
        list(insertions = 1, deletions = 1, substitutions = 1)
    if(!is.null(cost)) {
        ## Compatibility with agrep().
        cost <- as.list(cost)
        tab <- names(all_costs)
        pos <- pmatch(names(cost), tab)
        if(any(is.na(pos))) {
            warning("unknown cost components ignored")
        }
        all_costs[pos] <- cost[!is.na(pos)]
        ## Could add some sanity checking ...
    }

    .Internal(adist(x, y,
                    all_costs$insertions,
                    all_costs$deletions,
                    all_costs$substitutions,
                    counts, fixed, partial, ignore.case, useBytes))
}

## No longer used by adist(), but could be more generally useful ...
    
regquote <-
function(x)
    gsub("([*.?+^&\\[])", "\\\\\\1", x)
