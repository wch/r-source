# needs save lib pkg R_HOME
lib=$2
pkg=$3
R_HOME=$4

case $1 in
    CHECK) if test -r INSTALL.R; then R_SAVE_IMAGE=true; else R_SAVE_IMAGE=false; fi;;
    *) R_SAVE_IMAGE=$1;;
esac
export R_SAVE_IMAGE

if ${R_SAVE_IMAGE}; then
    echo "  save image"
    if test -s "R_PROFILE.R"; then true
    else
	echo "options(echo=FALSE)" > R_PROFILE.R
    fi
    R_PROFILE=./R_PROFILE.R
    export R_PROFILE
    (echo " .lib.loc <- c(\"${lib}\", .lib.loc)"; 
      cat ${lib}/${pkg}/R/${pkg};
      echo "rm(.lib.loc)") | ${R_HOME}/bin/Rterm --save --silent \
        || (echo "Execution of package source for ${pkg} failed"; exit 1)
    mv .RData ${lib}/${pkg}/R/all.rda
    mv ${lib}/${pkg}/R/${pkg} ${lib}/${pkg}/R/${pkg}.R
    if test -s INSTALL.R; then cp INSTALL.R ${lib}/${pkg}/R/${pkg}
    else 
	cat > ${lib}/${pkg}/R/${pkg} <<EOF
.First.lib <- function(libname, pkgname) {
  fullName <- paste("package", pkgname, sep=":")
  myEnv <- pos.to.env(match(fullName, search()))
  dataFile <- file.path(libname, pkgname, "R", "all.rda")
  rm(.First.lib, envir = myEnv)
  load(dataFile, myEnv)
  if(exists(".First.lib", envir = myEnv, inherits = FALSE)) {
    f <- get(".First.lib", envir = myEnv, inherits = FALSE)
    if(is.function(f))
      f(libname, pkgname)
    else
      stop(paste("package \"", pkgname, "\"has a non-function .First", sep=""))
  }
}
EOF
    fi
fi
