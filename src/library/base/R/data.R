## Was in  system.unix.R --  now system-independent
## thanks to Guido's  .Platform$show.data(.) idea.
data <-
function (..., list = character(0),
          package = c(.packages(), .Autoloaded),
          lib.loc = .lib.loc, verbose = .Options$verbose) 
{
  names <- c(as.character(substitute(list(...))[-1]), list)
  ## && !is.character(package))
  if (!missing(package)) 
    if (is.name(y <- substitute(package))) 
      package <- as.character(y)
  found <- FALSE
  fsep <- .Platform$file.sep
  if (length(names) == 0) 
    show.data(package, lib.loc)
  else for (name in names) {
    files <- list.files(system.file("data", pkg = package, lib = lib.loc),
                        full = TRUE)
    files <- files[grep(name, files)]
    found <- FALSE
    if (length(files) > 0) {
      subpre <- paste(".*", fsep, sep = "")
      for (file in files) {
        if (verbose) 
          cat("name=", name, ":\t file= ...", fsep, sub(subpre, 
                                    "", file), "::\t", sep = "")
        if (found) 
          break
        found <- TRUE
        ext <- sub(".*\\.", "", file)
        ## make sure the match is really for `name.ext'
        ## otherwise
        if (sub(subpre, "", file) != paste(name, ".", 
                 ext, sep = "")) 
          found <- FALSE
        else switch(ext,
                    R = ,
                    r = source(file),
                    RData = , 
                    rdata = ,
                    rda = load(file, envir = .GlobalEnv),
                    TXT = ,
                    txt = , 
                    tab = assign(name, read.table(file, header = TRUE), 
                      env = .GlobalEnv), CSV = ,
                    csv = assign(name, 
                      read.table(file, header = TRUE, sep = ";"), 
                      env = .GlobalEnv), found <- FALSE)
        if (verbose) 
          cat(if (!found) 
              "*NOT* ", "found\n")
      }
    }
    if (!found) 
      warning(paste("Data set `", name, "' not found", 
                    sep = ""))
  }
  invisible(names)
}

### fsep is unused! BDR
show.data <-
  function (package, lib.loc, fsep=.Platform$file.sep) 
{
  ## give `index' of all possible data sets
  file <- tempfile("R.")
  file.create(file)
  on.exit(unlink(file))
  first <- TRUE
  for (lib in lib.loc) for (pkg in package) {
    INDEX <- system.file("data", "index.doc", pkg = pkg, lib = lib)
    if (INDEX != "") {
      cat(paste(ifelse(first, "", "\n"), "Data sets in package `", 
                pkg, "':\n\n", sep = ""), file = file, append = TRUE)
      file.append(file, INDEX)
      first <- FALSE
    }
  }
  if (first) 
    stop("No data sets found")
  else file.show(file)
}
