###--- This "make-<name>" script is only used to create the real script <name> :

sink("no-segfault.R")

cat("options(error.halt = FALSE)",
    "# don't stop on error in batch\n##~~~~~~~~~~~~~~\n")

core.pkgs <- ##.packages(all=TRUE,lib.loc = .lib.loc[length(.lib.loc)])
    c("base", "eda", "lqs", "modreg", "mva", "stepfun", "ts")
stop.list <- vector("list", length(core.pkgs))
names(stop.list) <- core.pkgs

## -- Stop List for "base" :
stop.list[["base"]] <-
    if(nchar(getenv("R_TESTLOTS"))) {## SEVERE TESTING, try almost ALL
	"bug.report"
    } else {
	inet.list <- c(apropos("download\."),
                       apropos("^url\."), apropos("\.url"),
                       "CRAN.packages", "update.packages")
	socket.fun <- apropos("socket")# those *do* segfault...
	## "Interactive" ones:
	dev.int <- c("X11", "windows", "macintosh")
        edit.int <- c("fix", "edit", "vi", "emacs", "pico", "xemacs", "xedit")
	misc.int <- c("bug.report", "browser",
		      "data.entry", "dataentry", "de", apropos("^de\."))
	c(inet.list, socket.fun, dev.int, edit.int, misc.int)
    }

for (pkg in core.pkgs) {
  cat("### Package ", pkg, "\n",
      "###	   ", rep("~",nchar(pkg)), "\n", collapse="", sep="")
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
	  cat("f()\nf(NULL)\nf(NULL,NULL)\n",
	      "f(list())\nf(logical(0))\nf(character(0))\n",
	      "f(FALSE)\n",
	      sep="")
      }
  }
  if(pkg != "base") {
      detach(pos=this.pos)
      cat("detach(pos=",this.pos,")\n")
  }

  cat("\n##__________\n\n")
}

