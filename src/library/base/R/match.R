## till R 1.1.1:
match <- function(x, table, nomatch=NA)
    .Internal(match(as.character(x), as.character(table), nomatch))
## New:
match <- function(x, table, nomatch=NA, incomparables = FALSE) {
    if(!is.logical(incomparables) || incomparables)
        .NotYetUsed("incomparables != FALSE")
    .Internal(match(if(is.factor(x)) as.character(x) else x,
                    if(is.factor(table)) as.character(table) else table,
                    nomatch))
}

match.call <-
    function(definition=NULL, call=sys.call(sys.parent()), expand.dots=TRUE)
    .Internal(match.call(definition,call,expand.dots))

pmatch <-
    function(x, table, nomatch=NA, duplicates.ok=FALSE)
{
    y <- .Internal(pmatch(x,table,duplicates.ok))
    y[y == 0] <- nomatch
    y
}

"%in%" <- function(x, table) match(x, table, nomatch = 0) > 0

match.arg <- function (arg, choices) {
    if (missing(choices)) {
	formal.args <- formals(sys.function(sys.parent()))
	choices <- eval(formal.args[[deparse(substitute(arg))]])
    }
    if (all(arg == choices)) return(choices[1])
    i <- pmatch(arg, choices)
    if (is.na(i))
	stop("'arg' should be one of ", paste(choices, collapse = ", "))
    if (length(i) > 1) stop("there is more than one match in 'match.arg'")
    choices[i]
}

charmatch <-
    function(x, table, nomatch=NA)
{
    y <- .Internal(charmatch(x,table))
    y[is.na(y)] <- nomatch
    y
}

char.expand <-
    function(input, target, nomatch = stop("no match"))
{
    if(length(input) != 1)
	stop("'input' must have length 1")
    if(!(is.character(input) && is.character(target)))
	stop("'input' and 'target' must be character vectors")
    y <- .Internal(charmatch(input,target))
    if(any(is.na(y))) eval(nomatch)
    target[y]
}
