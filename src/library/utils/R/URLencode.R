URLencode <- function(URL, reserved = TRUE)
{
    OK <- if(reserved) "[^A-Za-z0-9$\\-_.+!*'(),;/?:@=&]"
          else "[^A-Za-z0-9$\\-_.+!*'(),]"
    x <- strsplit(URL, "")[[1]]
    z <- grep(OK, x)
    if(length(z)) {
        y <- sapply(x[z], function(x) as.character(charToRaw(x)))
        x[z] <- paste("%", y, sep="")
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
            y <- sum((as.integer(x[i+1:2]) - 48) * c(16, 1))
            out <- c(out, as.raw(as.character(y)))
            i <- i + 3
        }
    }
    rawToChar(out)
}


