adist <-
function(x, y = x, cost = NULL, counts = FALSE,
         partial = FALSE, ignore.case = FALSE, useBytes = FALSE)
{
    pattern <- if(!identical(partial, TRUE))
        sprintf("^%s$", regquote(x))
    else x

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

    .Internal(adist(pattern, x, y,
                    all_costs$insertions,
                    all_costs$deletions,
                    all_costs$substitutions,
                    counts, partial, ignore.case, useBytes))
}

## Use by adist() for now, but could be more generally useful.
    
regquote <-
function(x)
    gsub("([*.?+^&\\[])", "\\\\\\1", x)
