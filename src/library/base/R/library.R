require <- function(name, quietly = FALSE) {
  name <- as.character(substitute(name)) # allowing "require(eda)"
  if (!exists(".Provided", inherits = TRUE)) 
    assign(".Provided", character(0), envir = .GlobalEnv)
  if (is.na(match(paste("package", name, sep = ":"), search()))
      && is.na(match(name, .Provided))) {
    if (!quietly)
      cat("Loading required package:", name, "\n")
    library(name, char = TRUE, logical = TRUE)
  }
  else
    TRUE
}

provide <- function(name) {
  if (!exists(".Provided", inherits = TRUE)) 
    assign(".Provided", character(0), envir = .GlobalEnv)
  if (missing(name)) 
    .Provided
  else {
    name <- as.character(substitute(name))
    if (is.na(match(name, .packages())) &&
	is.na(match(name, .Provided))) {
      assign(".Provided", c(name, .Provided), envir = .GlobalEnv)
      TRUE
    }
    else
      FALSE
  }
}

.packages <- function() {
  s <- search()
  return(invisible(substring(s[substr(s, 1, 8) == "package:"], 9)))
}  
