###--- This "make-<name>" script is only used to create the real script <name> :

###--- We need to use such a long "real script" instead of a for loop,
###--- because "error --> jump_to_toplevel", i.e., outside any loop.

core.pkgs <- .packages(all=TRUE,lib.loc = .lib.loc[length(.lib.loc)])
## c("base", "eda", "lqs", "modreg", "mva", "stepfun", "ts", "nls","splines")
stop.list <- vector("list", length(core.pkgs))
names(stop.list) <- core.pkgs

## -- Stop List for "base" :
edit.int <- c("fix", "edit", "vi", "emacs", "pico", "xemacs", "xedit")
misc.int <- c("browser", "bug.report", "menu")
stop.list[["base"]] <-
    if(nchar(getenv("R_TESTLOTS"))) {## SEVERE TESTING, try almost ALL
	c(edit.int, misc.int)
    } else {
	inet.list <- c(apropos("download\."),
		       apropos("^url\."), apropos("\.url"),
		       paste(c("CRAN", "install", "update"),"packages",sep="."))
	socket.fun <- apropos("socket")
	## "Interactive" ones:
	dev.int <- c("X11", "windows", "macintosh")
	## print.plot() will print a blank page on the printer and is
	## deprecated anyway --pd
	misc.2 <- c("help.start", "print.plot",
		    "gctorture", "q", "quit",
                    ## now `works': "function",
		    "data.entry", "dataentry", "de", apropos("^de\."))
	c(inet.list, socket.fun, dev.int, edit.int, misc.int, misc.2)
    }

sink("no-segfault.R")

cat('options(pager = "cat", error=expression(NULL))',
    "# don't stop on error in batch\n##~~~~~~~~~~~~~~\n")

cat("c0 <- character(0)\n",
    "l0 <- logical(0)\n",
    "df0 <- as.data.frame(c0)\n", sep="")

for (pkg in core.pkgs) {
  cat("### Package ", pkg, "\n",
      "###         ", rep("~",nchar(pkg)), "\n", collapse="", sep="")
  if(pkg == "base") {
      this.pos <- length(search())
  }
  else {
      library(pkg, character = TRUE)
      cat("library(",pkg,", character = TRUE)\n")
      this.pos <- 2
  }

  for(nm in ls(pos = this.pos)) {
      if(!(nm %in% stop.list[[pkg]]) &&
	 is.function(f <- get(nm,pos = this.pos))) {
	  cat("\n## ", nm, " :\n")
	  cat("f <- get(\"",nm,"\", pos = ", this.pos, ")\n", sep="")
	  cat("f()\nf(NULL)\nf(,NULL)\nf(NULL,NULL)\n",
	      "f(list())\nf(l0)\nf(c0)\nf(df0)\nf(F)\n",
	      "f(list(),list())\nf(l0,l0)\nf(c0,c0)\nf(df0,df0)\nf(F,F)\n",
	      sep="")
      }
  }
  if(pkg != "base") {
      detach(pos=this.pos)
      cat("detach(pos=",this.pos,")\n")
  }

  cat("\n##__________\n\n")
}

