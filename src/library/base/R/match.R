match <- function(x, table, nomatch=NA) .Internal(match(x, table, nomatch))

match.call <-
function(definition=NULL, call=sys.call(sys.parent()), expand.dots=T)
        .Internal(match.call(definition,call,expand.dots))

pmatch <-
function(x, table, nomatch=NA, duplicates.ok=FALSE)
{
	y <- .Internal(pmatch(x,table,duplicates.ok))
	y[y == 0] <- nomatch
	y
}

"%in%" <-
function(x, y)
match(x, y, nomatch = 0) > 0

match.arg <-
function(arg, choices)
{
  if (missing(choices)) {
    formal.args <- formals(sys.function(sys.parent()))
    choices <- eval(formal.args[[deparse(substitute(arg))]])
  }
  i <- pmatch(arg, choices)
  if (is.na(i))
    stop(paste("ARG should be one of",
	       paste(choices, collapse=", "), sep = " "))
  else if (i == 0)
    if (arg == choices)
      rval <- choices[1]
    else
      stop("there is more than one match in match.arg")
  else
    rval <- choices[i]
  return(rval)
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
		stop("char.expand: input must have length 1")
	if(!(is.character(input) && is.character(target)))
		stop("char.expand: input must be character")
	y <- .Internal(charmatch(input,target))
	if(any(is.na(y))) eval(nomatch)
	target[y]
}
