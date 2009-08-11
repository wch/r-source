SweaveTeXFilter <- function(ifile, encoding = "unknown")
{
    lines <- readLines(ifile, encoding = encoding, warn = FALSE)

    TEXT <- 1L
    CODE <- 0L

    recs <- rbind( data.frame(line=grep("^@", lines), type=TEXT),
                   data.frame(line=grep("^<<(.*)>>=.*", lines), type=CODE))
    recs <- recs[order(recs$line),]
    last <- 0L
    state <- TEXT
    for (i in seq_len(nrow(recs))) {
    	line <- recs$line[i]
    	if (state == CODE)
    	    lines[(last+1L):line] <- ""
    	else
    	    lines[line] <- ""
    	state <- recs$type[i]
    	last <- line
    }
    lines
}
