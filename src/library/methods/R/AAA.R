pkg <- Sys.getenv("R_PACKAGE_DIR")
if(nchar(pkg) > 0) {
  cat("Looks like we're doing INSTALL:  will dynload the package DLL\n")
  dyn.load(file.path(pkg, "libs",
                     paste("methods", .Platform$file.sep, sep="")))
  cat("dynamic load complete\n")
}
rm(pkg)

