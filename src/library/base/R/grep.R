grep <-
function(pattern, x, ignore.case = FALSE, extended = TRUE, perl = FALSE,
         value = FALSE)
{
    if(perl)
        .Internal(grep.perl(pattern, x, ignore.case, value))
    else
        .Internal(grep(pattern, x, ignore.case, extended, value))
}

sub <-
function(pattern, replacement, x, ignore.case = FALSE, extended = TRUE,
         perl = FALSE)
{
    if(perl)
        .Internal(sub.perl(pattern, replacement, x, ignore.case))
    else
        .Internal(sub(pattern, replacement, x, ignore.case, extended))
}

gsub <-
function(pattern, replacement, x, ignore.case = FALSE, extended = TRUE,
         perl = FALSE)
{
    if(perl)
        .Internal(gsub.perl(pattern, replacement, x, ignore.case))
    else
        .Internal(gsub(pattern, replacement, x, ignore.case, extended))
}

regexpr <-
function(pattern, text, extended = TRUE, perl = FALSE)
{
    if(perl)
        .Internal(regexpr.perl(pattern, text))
    else
        .Internal(regexpr(pattern, text, extended))
}

agrep <-
function(pattern, x, ignore.case = FALSE, value = FALSE,
         max.distance = 0.1)
{
    if(!is.list(max.distance))
        max.insertions <- max.deletions <- max.substitutions <-
            max.distance
    else {
        ## partial matching
        table <- c("all", "deletions", "insertions", "substitutions")
        ind <- pmatch(names(max.distance), table)
        if(any(is.na(ind)))
            warning("incorrect specifiction of match distance")
        names(max.distance) <- table[ind]
        ## extract restrictions
        if(is.null(max.distance$all))
            max.distance$all <- 0.1
        max.insertions <- max.deletions <- max.substitutions <-
            max.distance <- max.distance$all
        if(!is.null(max.distance$deletions))
            max.deletions <- max.distance$deletions
        if(!is.null(max.distance$insertions))
            max.insertions <- max.distance$insertions
        if(!is.null(max.distance$substitutions))
            max.substitutions <- max.distance$substitutions
    }

    ## transform percentages
    n <- nchar(pattern)
    if(max.distance < 1)
        max.distance <- ceiling(n * max.distance)
    if(max.deletions < 1)
        max.deletions <- ceiling(n * max.deletions)
    if(max.insertions < 1)
        max.insertions <- ceiling(n * max.insertions)
    if(max.substitutions < 1)
        max.substitutions <- ceiling(n * max.substitutions)

    .Internal(agrep(pattern, x, ignore.case, value, max.distance,
                    max.deletions, max.insertions, max.substitutions))
}
