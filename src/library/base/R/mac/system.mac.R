system <- function(call, intern = FALSE)
    .Internal(system(call, intern))

dir.create <- function(path)
    invisible(.Internal(dir.create(path)))

tempfile <- function(pattern = "file") .Internal(tempfile(pattern))

unlink <- function(x, recursive=FALSE)
    invisible(.Internal(unlink(x, recursive)))

help.start <- function(gui = "irrelevant", browser = "irrelevant")
{
    a <- system.file("rmac.html", pkg="doc:html", lib=R.home())
    if (a == "")
        stop("I can't find the html help")
    else {
        cat("If nothing happens, you have to open `", a ,"' yourself\n")
        .Internal(help.start());
    }
    invisible("")
}


zip.file.extract <- function(file, zipname="R.zip")
{
    if(file.exists(file)) return(file)  # if the file exists, do not
                                        # replace it

    ofile <- gsub("::::", ":", file)
    path <- sub("[^:]*$","", ofile)  # changed "/" to ":"
    topic <- substr(ofile, nchar(path)+1, 1000)

    if(file.exists(file.path(path, zipname,fsep=""))) {

        tempdir <- sub("[^:]*$","", tempfile())
        if((unzip <- getOption("unzip")) != "mac.unzip") {
            if(!system(paste(unzip, ' -oq "',
                             file.path(path, zipname), '" ', topic,
                             " -d ", tempdir, sep=""), invisible = TRUE))
                file <- paste(tempdir,  topic, sep="")
        }
        else  {    # mac.unzip
            rc <- .Internal(int.unzip(file.path(path, zipname, fsep=""),
                                      topic, path))
            if (rc != 0) warning("Error in MacUnZip")
        }
    }
    file
}

demo <- function(topic, device = getOption("device")) {
    if (is.character(device)) device <- get(device)
    Topics <-cbind(graphics	= c("graphics", "graphics.R",	"G"),
		   image	= c("graphics", "image.R",	"G"),
		   lm.glm	= c("models",	"lm+glm.R",	"G"),
		   glm.vr	= c("models",	"glm-v+r.R",	""),
		   nlm		= c("nlm",	"valley.R",	""),
		   recursion	= c("language", "recursion.R",	"G"),
		   scoping	= c("language", "scoping.R",	""),
		   is.things	= c("language", "is-things.R",	"")
		   )
    dimnames(Topics)[[1]] <- c("dir", "file", "flag")
    topic.names <- dimnames(Topics)[[2]]
    demo.help <- function() {
	cat("Use `demo(topic)' where choices for argument `topic' are:\n")
	cbind(topics = topic.names)
    }
    if(missing(topic)) return(demo.help())
    topic <- substitute(topic)
    if (!is.character(topic)) topic <- deparse(topic)[1]
    i.top <- pmatch(topic, topic.names)
    if (is.na(i.top) || i.top == 0) {
	cat("unimplemented `topic' in demo.\n")
	print(demo.help())
	stop()
    } else {
	topic <- topic.names[i.top]
	cat("\n\n\tdemo(",topic,")\n\t---- ",rep("~",nchar(topic)),
	    "\n\nType  <Return>	 to start : ",sep="")
	readline()
	if(dev.cur()<=1 && Topics["flag",i.top] == "G")
	    device()
	source(file.path(R.home(),
                         "demos",
                         Topics["dir",  i.top],
                         Topics["file", i.top]),
	       echo = TRUE, max.deparse.length=250)
    }
}




