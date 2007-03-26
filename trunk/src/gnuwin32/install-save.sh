# needs save lib pkg R_HOME BUILD RX_EXE DPKG
lib=$2
pkg=$3
R_HOME=$4
BUILD=$5
RX_EXE=$6
DPKG=$7

R_SAVE_IMAGE=$1
export R_SAVE_IMAGE
if test -r install.R; then 
    echo "WARNING: use of install.R is no longer supported"
fi
if test -r R_PROFILE.R; then 
    echo "WARNING: use of R_PROFILE.R is no longer supported"
fi

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

if test "x${R_SAVE_IMAGE}" = "xtrue"; then
    echo "  save image"
    if test "x${BUILD}" = "xCROSS"; then
        mkdir -p ${lib1}/${pkg}
        cp -r ${lib}/${pkg}/DESCRIPTION ${lib}/${pkg}/R ${lib1}/${pkg}
        if test -f NAMESPACE; then
          cp ${lib}/${pkg}/NAMESPACE ${lib1}/${pkg}
        fi
    fi
    save_image_defaults="list(compress=TRUE, safe=FALSE)"
    code_file="${DPKG}/R/${pkg}"
    rda_file="${DPKG}/R/all.rda"
    if test -f NAMESPACE; then
	pkg_name=`basename ${DPKG}`
        code_cmd="echo invisible(.libPaths(c(.Library,\"${lib1}\",.libPaths()))); .getRequiredPackages(); saveNamespaceImage(\"${pkg_name}\", \"${rda_file}\", \"${lib1}\")"
        loader_file=nsrdaload.R
        R_SAVE_EXE=""
    else
        code_cmd="cat ${code_file}"
        loader_file=firstlib.R
        R_SAVE_EXE="--save"
    fi
    (echo "options(save.image.defaults=${save_image_defaults})"; \
      echo "invisible(.libPaths(c(.Library,\"${lib}\",.libPaths()))); .getRequiredPackages()"; \
      ${code_cmd}) | LC_ALL=C ${R_EXE} ${R_SAVE_EXE}
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
fi
