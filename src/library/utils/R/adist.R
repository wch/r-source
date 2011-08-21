adist <-
function(x, y = x, cost = NULL, partial = FALSE,
         all = FALSE, ignore.case = FALSE, useBytes = FALSE)
{
    if(!identical(partial, TRUE)) {
        nmx <- names(x)
        x <- sprintf("^%s$", regquote(x))
        names(x) <- nmx
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
                    partial, all, ignore.case, useBytes))
}
    
regquote <-
function(x)
    gsub("([*.?+^&\\[])", "\\\\\\1", x)
