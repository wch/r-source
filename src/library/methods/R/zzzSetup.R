 .saveImage <- FALSE
if(Sys.getenv("R_SAVE_IMAGE") == "true") {
  ## we're running the package code to create and save an image
  .First.lib("methods", "methods", .GlobalEnv)
} else
  library.dynam("methods", "methods")
