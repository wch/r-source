SweaveTeXFilter <-
function(ifile, encoding = "unknown") {
    
    lines <- readLines(ifile, encoding = encoding, warn = FALSE)
    
    TEXT <- 1
    CODE <- 0
    
    recs <- rbind( data.frame(line=grep("^@", lines), type=TEXT),
                   data.frame(line=grep("^<<(.*)>>=.*", lines), type=CODE))
    recs <- recs[order(recs$line),]
    last <- 0
    state <- TEXT
    for (i in seq.int(1, nrow(recs))) {
    	line <- recs$line[i]
    	if (state == CODE)
    	    lines[(last+1):line] <- ""
    	else
    	    lines[line] <- ""
    	state <- recs$type[i]
    	last <- line
    }
    lines
}
