pkg <- Sys.getenv("R_PACKAGE_DIR")
if(nchar(pkg) > 0) {
  cat("Looks like we're doing INSTALL:  will dynload the package .so file\n")
  dyn.load(file.path(pkg, "libs", "methods.so"))
  cat("dynamic load complete\n")
} 
rm(pkg)

