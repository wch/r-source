# needs save lib pkg R_HOME BUILD RX_EXE
lib=$2
pkg=$3
R_HOME=$4
BUILD=$5
RX_EXE=$6

case $1 in
    CHECK|'') if test -r install.R; then R_SAVE_IMAGE=true; else R_SAVE_IMAGE=false; fi;;
    *) R_SAVE_IMAGE=$1;;
esac
export R_SAVE_IMAGE

## R_START_PACKAGES is set to NULL during bootstrapping
R_DEFAULT_PACKAGES=${R_START_PACKAGES}
export R_DEFAULT_PACKAGES
if test "x${BUILD}" = "xCROSS"; then
R_EXE="${RX_EXE} --slave --no-site-file --no-init-file"
## we have to install into a temporary dir to avoid libpath problems
lib1=/tmp/RtmpWin
else
R_EXE="${R_HOME}/bin/Rterm --slave --no-site-file --no-init-file"
lib1=${lib}
fi

## This is apparently needed, but no one told the Windows maintainers.
R_PACKAGE_NAME=${pkg}
export R_PACKAGE_NAME

if ${R_SAVE_IMAGE}; then
    echo "  save image"
    if test "x${BUILD}" = "xCROSS"; then
        mkdir -p ${lib1}/${pkg}
        cp -r ${lib}/${pkg}/DESCRIPTION ${lib}/${pkg}/R ${lib1}/${pkg}
        if test -f NAMESPACE; then
          cp ${lib}/${pkg}/NAMESPACE ${lib1}/${pkg}
        fi
    fi
    save_image_defaults="list(compress=TRUE, safe=FALSE)"
    code_file="${lib}/${pkg}/R/${pkg}"
    rda_file="${lib}/${pkg}/R/all.rda"
    if test -f NAMESPACE; then
        code_cmd="echo invisible(.libPaths(\"${lib1}\")); .getRequiredPackages(); saveNamespaceImage(\"${pkg}\", \"${rda_file}\", \"${lib1}\")"
        loader_file=nsrdaload.R
        R_SAVE_EXE=""
    else
        code_cmd="cat ${code_file}"
        loader_file=firstlib.R
        R_SAVE_EXE="--save"
    fi
    (echo "options(save.image.defaults=${save_image_defaults})"; \
      if test -s R_PROFILE.R; then cat R_PROFILE.R; fi; \
      echo "invisible(.libPaths(\"${lib1}\")); .getRequiredPackages()"; \
      ${code_cmd}) | ${R_EXE} ${R_SAVE_EXE}
    if test ${?} -ne 0; then
      echo "execution of package source for '${pkg}' failed"
      exit 1
    fi
    if test ! -f NAMESPACE; then
        mv .RData ${rda_file}
    fi
    if test "x${BUILD}" = "xCROSS"; then
        rm -rf ${lib1}
    fi
    ## we used to install the dumped code but this seems a waste of space
    rm "${code_file}"
    # mv "${code_file}" "${code_file}.R"
    cat "${R_HOME}/share/R/${loader_file}" > "${code_file}"
    ## if install.R is non-empty, arrange to evaluate the R code it
    ## contains after the package source (maybe for some kind of
    ## cleanup).
    if test -s install.R; then
      cat install.R >> ${lib}/${pkg}/R/${pkg}
    fi
fi
