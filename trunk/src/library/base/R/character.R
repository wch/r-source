strsplit <- function(x, split, extended = TRUE, fixed = FALSE, perl = FALSE)
    .Internal(strsplit(x, as.character(split), as.logical(extended),
                       as.logical(fixed), as.logical(perl)))

substr <- function(x, start, stop)
    .Internal(substr(x, as.integer(start), as.integer(stop)))

substring <- function(text,first,last=1000000)
{
    storage.mode(text) <- "character"
    n <- max(lt <- length(text), length(first), length(last))
    if(lt && lt < n) text <- rep(text, length.out = n)
    substr(text, first, last)
}

"substr<-" <- function(x, start, stop, value)
    .Internal(substrgets(x, as.integer(start), as.integer(stop), value))

"substring<-" <- function(text, first, last=1000000, value)
{
    "substr<-"(text, first, last, value)
}

abbreviate <-
    function(names.arg, minlength = 4, use.classes = TRUE, dot = FALSE)
{
    ## we just ignore use.classes
    if(minlength <= 0)
	return(rep.int("", length(names.arg)))
    ## need to remove leading/trailing spaces before we check for dups
    ## This is inefficient but easier than modifying do_abbrev (=> FIXME !)
    names.arg <- sub("^ +", "", sub(" +$", "", as.character(names.arg)))
    dups <- duplicated(names.arg)
    old <- names.arg
    if(any(dups))
	names.arg <- names.arg[!dups]
    dup2 <- rep.int(TRUE, length(names.arg))
    x <- these <- names.arg
    repeat {
	ans <- .Internal(abbreviate(these,minlength,use.classes))
	x[dup2] <- ans
	dup2 <- duplicated(x)
	if(!any(dup2))
	    break
	minlength <- minlength+1
	dup2 <- dup2 | match(x, x[dup2], 0)
	these <- names.arg[dup2]
    }
    if(any(dups))
	x <- x[match(old,names.arg)]
    if(dot) { # add "." where we did abbreviate:
        chgd <- x != old
	x[chgd] <- paste(x[chgd],".",sep = "")
    }
    names(x) <- old
    x
}

make.names <- function(names, unique = FALSE, allow_ = TRUE)
{
    names <- .Internal(make.names(as.character(names), allow_))
    if(unique) names <- make.unique(names)
    names
}

make.unique <- function (names, sep = ".") .Internal(make.unique(names, sep))

chartr <- function(old, new, x) .Internal(chartr(old, new, x))
tolower <- function(x) .Internal(tolower(x))
toupper <- function(x) .Internal(toupper(x))

casefold <- function(x, upper = FALSE)
    if(upper) toupper(x) else tolower(x)

sQuote <- function(x) {
    if(length(x) == 0) return(character())
    if(l10n_info()$"UTF-8")
        paste("\xe2\x80\x98", x, "\xe2\x80\x99", sep = "")
    else
        paste("'", x, "'", sep = "")
}
dQuote <- function(x) {
    if(length(x) == 0) return(character())
    if(l10n_info()$"UTF-8")
        paste("\xe2\x80\x9c", x, "\xe2\x80\x9d", sep = "")
    else
        paste("\"", x, "\"", sep = "")
}
