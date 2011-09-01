adist <-
function(x, y = x, cost = NULL, counts = FALSE,
         partial = FALSE, ignore.case = FALSE, useBytes = FALSE)
{
    nx <- length(x)
    ny <- length(y)

    lpositions <-
        as.integer(rep.int(seq(from = 0L, length.out = nx),
                           ny))
    rpositions <-
        as.integer(rep.int(seq(from = nx, length.out = ny),
                           rep.int(nx, ny)))
    if(!identical(partial, TRUE)) {
        ind <- outer(nchar(x), nchar(y), `<`)
        if(any(ind)) {
            tmp <- lpositions[ind]
            lpositions[ind] <- rpositions[ind]
            rpositions[ind] <- tmp
        }
    }
    rpositions <- split(rpositions, lpositions)
    lpositions <- as.integer(names(rpositions))

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

    .Internal(adist(x, y, lpositions, rpositions,
                    all_costs$insertions,
                    all_costs$deletions,
                    all_costs$substitutions,
                    counts, partial, ignore.case, useBytes))
}

## No longer used by adist(), but could be more generally useful ...
    
regquote <-
function(x)
    gsub("([*.?+^&\\[])", "\\\\\\1", x)
