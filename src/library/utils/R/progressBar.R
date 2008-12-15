txtProgressBar <-
    function(min = 0, max = 1, initial = 0, char = "=",
             width = NA, title, label, style = 1)
{
    if(! style %in% 1L:3L) style <- 1
    .val <- initial
    .killed <- FALSE
    .nb <- 0L
    .pc <- 0L
    nw <- nchar(char, "w")
    if(is.na(width)) {
        width <- getOption("width")
        if(style == 3L) width <- width - 10L
        width <- trunc(width/nw)
    }

    up1 <- function(value) {
        if(!is.finite(value) || value < min || value > max) return()
        .val <<- value
        nb <- round(width*(value - min)/(max - min))
        if(.nb < nb) {
            cat(paste(rep.int(char, nb-.nb), collapse=""))
            flush.console()
        } else if (.nb > nb) {
            cat(paste(c("\r", rep.int(" ", .nb*nw)), collapse=""))
            cat(paste(c("\r", rep.int(char, nb)), collapse=""))
            flush.console()
        }
        .nb <<- nb
    }

    up2 <- function(value) {
        if(!is.finite(value) || value < min || value > max) return()
        .val <<- value
        nb <- round(width*(value - min)/(max - min))
        if(.nb <= nb) {
            cat(paste("\r", rep.int(char, nb), collapse=""))
            flush.console()
        } else {
            cat(paste(c("\r", rep.int(" ", .nb*nw)), collapse=""))
            cat(paste(c("\r", rep.int(char, nb)), collapse=""))
            flush.console()
        }
        .nb <<- nb
    }

    up3 <- function(value) {
        if(!is.finite(value) || value < min || value > max) return()
        .val <<- value
        nb <- round(width*(value - min)/(max - min))
        pc <- round(100*(value - min)/(max - min))
        if(nb == .nb && pc == .pc) return()
        cat(paste(c("\r  |", rep.int(" ", nw*width+6)), collapse=""))
        cat(paste(c("\r  |",
                    rep.int(char, nb),
                    rep.int(" ", nw*(width-nb)),
                    sprintf("| %3d%%", pc)
                    ), collapse=""))
        flush.console()
        .nb <<- nb
        .pc <<- pc
    }

    getVal <- function() .val
    kill <- function()
        if(!.killed) {
            cat("\n")
            flush.console()
            .killed <<- TRUE
        }
    up <- switch(style, up1, up2, up3)
    if(initial > min) up(initial)
    structure(list(getVal=getVal, up=up, kill=kill),
              class = "txtProgressBar")
}

getTxtProgressBar <- function(pb)
{
    if(!inherits(pb, "txtProgressBar"))
       stop("'pb' is not from class \"txtProgressBar\"")
    pb$getVal()
}

setTxtProgressBar <- function(pb, value, title = NULL, label = NULL)
{
    if(!inherits(pb, "txtProgressBar"))
       stop("'pb' is not from class \"txtProgressBar\"")
    oldval <- pb$getVal()
    pb$up(value)
    invisible(oldval)
}

close.txtProgressBar <- function(con, ...)
{
    con$kill()
    invisible(NULL)
}
