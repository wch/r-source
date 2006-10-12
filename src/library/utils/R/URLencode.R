URLencode <- function(URL, reserved = FALSE)
{
    ## It is unsafe to use ranges here as collation is locale-dependent.
    ## We want to do this on characters and not on bytes.
    OK <- paste("[^-ABCDEFGHIJKLMNOPQRSTUVWXYZ",
		"abcdefghijklmnopqrstuvwxyz0123456789$_.+!*'(),",
		if(!reserved) ";/?:@=&", "]", sep="")
    x <- strsplit(URL, "")[[1]]
    z <- grep(OK, x)
    if(length(z)) {
        y <- sapply(x[z], function(x)
                    paste("%", as.character(charToRaw(x)), sep=""))
        x[z] <- y
    }
    paste(x, collapse="")
}

URLdecode <- function(URL)
{
    x <- charToRaw(URL)
    pc <- charToRaw("%")
    out <- raw(0)
    i <- 1
    while(i <= length(x)) {
        if(x[i] != pc) {
            out <- c(out, x[i])
            i <- i + 1
        } else {
            y <- as.integer(x[i+1:2])
            y[y > 96] <- y[y > 96] - 32 # a-f -> A-F
            y[y > 57] <- y[y > 57] - 7  # A-F
            y <- sum((y - 48) * c(16, 1))
            out <- c(out, as.raw(as.character(y)))
            i <- i + 3
        }
    }
    rawToChar(out)
}
