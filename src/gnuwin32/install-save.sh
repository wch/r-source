# needs save lib pkg R_HOME
lib=$2
pkg=$3
R_HOME=$4

case $1 in
    CHECK) if test -r install.R; then R_SAVE_IMAGE=true; else R_SAVE_IMAGE=false; fi;;
    *) R_SAVE_IMAGE=$1;;
esac
export R_SAVE_IMAGE

## This is apparently needed, but no one told the Windows maintainers.
R_PACKAGE_NAME=${pkg}
export R_PACKAGE_NAME

if ${R_SAVE_IMAGE}; then
    echo "  save image"
    save_image_defaults="list(compress=TRUE, safe=FALSE)"
    code_file="${lib}/${pkg}/R/${pkg}"
    rda_file="${lib}/${pkg}/R/all.rda"
    if test -f NAMESPACE; then
        code_cmd="echo saveNamespaceImage(\"${pkg}\", \"${rda_file}\", \"${lib}\")"
        loader_file=nsrdaload.R
        R_SAVE_EXE="${R_HOME}/bin/Rterm --slave --no-site-file --no-init-file"
    else
        code_cmd="cat ${code_file}"
        loader_file=firstlib.R
        R_SAVE_EXE="${R_HOME}/bin/Rterm --slave --save --no-site-file --no-init-file"
    fi
    (echo "options(save.image.defaults=${save_image_defaults})"; \
      if test -s R_PROFILE.R; then cat R_PROFILE.R; fi; \
      echo "invisible(.libPaths(c(\"${lib}\", .libPaths())))"; \
      ${code_cmd}) | ${R_SAVE_EXE} \
        || (echo "Execution of package source for ${pkg} failed"; exit 1)
    if test ! -f NAMESPACE; then
        mv .RData ${rda_file}
    fi
    mv "${code_file}" "${code_file}.R"
    cat "${R_HOME}/share/R/${loader_file}" > "${code_file}"
    ## if install.R is non-empty, arrange to evaluate the R code it
    ## contains after the package source (maybe for some kind of
    ## cleanup).
    if test -s install.R; then
      cat install.R >> ${lib}/${pkg}/R/${pkg}
    fi
fi
