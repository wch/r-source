strsplit <- function(x, split, extended = TRUE)
    .Internal(strsplit(x, as.character(split), as.logical(extended)))

substr <- function(x, start, stop)
    .Internal(substr(x, as.integer(start), as.integer(stop)))

substring <- function(text,first,last=1000000)
{
    storage.mode(text) <- "character"
    n <- max(lt <- length(text), length(first), length(last))
    if(lt < n) text <- rep(text, length = n)
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
    names.arg <- as.character(names.arg)
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

make.names <- function(names, unique = FALSE)
{
    names <- .Internal(make.names(as.character(names)))
    ## append `.' to keyword
    i <- is.element(names, c("for", "while", "repeat", "if",
                             "else", "function", "next", "break",
                             "TRUE", "FALSE", "NULL", "NA", "Inf", "NaN"))
    if(any(i)) names[i] <- paste(names[i], ".", sep = "")
    if(unique) names <- make.unique(names)
    names
}

# make.unique <- function (names, sep = ".")
# {
#     if (!is.character(names))
#         stop("names must be a character vector")
#     cnt <- 1
#     repeat {
#         i <- which(duplicated(names))
#         if (length(i) == 0) break
#         j <- i[!duplicated(names[i])]
#         newnames <- paste(names[j], cnt, sep=sep)
#         ok<- !(newnames %in% names) & !duplicated(newnames)
#         names[j][ok] <- newnames[ok]
#         if (identical(i, j) && all(ok)) break
#         cnt <- cnt + 1
#       }
#     names
# }

make.unique <- function (names, sep = ".") .Internal(make.unique(names, sep))

chartr <- function(old, new, x) .Internal(chartr(old, new, x))
tolower <- function(x) .Internal(tolower(x))
toupper <- function(x) .Internal(toupper(x))

casefold <- function(x, upper = FALSE)
    if(upper) toupper(x) else tolower(x)

sQuote <- function(x) {
    if(length(x) == 0) return(character())
    paste("'", x, "'", sep = "")
}
dQuote <- function(x) {
    if(length(x) == 0) return(character())
    paste("\"", x, "\"", sep = "")
}
