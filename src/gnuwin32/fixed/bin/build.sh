#
# ${R_HOME}/bin/build

revision='$Revision: 1.1.2.2 $'
version=`set - ${revision}; echo ${2}`
version="R package builder ${version}

Copyright (C) 2000 R Development Core Team.
There is NO warranty.  You may redistribute this software under the
terms of the GNU General Public License.
For more information about these matters, see the files named COPYING."

usage="Usage: sh ${R_HOME}/bin/build.sh [options] pkgdirs

Build R packages from package sources in the directories specified by
pkgdirs.  A variety of diagnostic checks and cleanups are performed
prior to building the packages.

If necessary for passing the checks, use the \`--vsize' and \`--nsize'
options to increase R's memory (\`--vanilla' is used by default).

Options:
  -d, --debug		turn on shell debugging (set -x)
  -h, --help		print short help message and exit
  -v, --version		print version info and exit
  --vsize=N		set R's vector heap size to N bytes
  --nsize=N		set R's number of cons cells to N
  --force		force overwriting of (index) files

Email bug reports to <r-bugs@lists.r-project.org>."

R_opts="--vanilla"
debug=false
force=false
pkgs=

## Parse argument command line
while test -n "${1}"; do
  case ${1} in
    -h|--help)
      echo "${usage}"; exit 0 ;;
    -v|--version)
      echo "${version}"; exit 0 ;;
    -d|--debug)
      debug=true ;;
    --force)
      force=true ;;
    --nsize=*)
      R_opts="{R_opts} ${1}" ;;
    --vsize=*)
      R_opts="{R_opts} ${1}" ;;
    *)
      pkgs="${pkgs} ${1}" ;;
  esac
  shift
done

start_dir=`pwd`
stars="*"

## Determine whether echo can suppress newlines.
if echo "testing\c" | grep c >/dev/null; then
  if echo -n "testing" | sed s/-n/xn/ | grep xn >/dev/null; then
    ECHO_N= ECHO_C= ECHO_T='	'
  else
    ECHO_N=-n ECHO_C= ECHO_T=
  fi
else
  ECHO_N= ECHO_C='\c' ECHO_T=
fi

## A few useful output functions
checking () {
  if ${debug}; then
    echo "${stars} checking $@ ..."
  else
    echo ${ECHO_N} "${stars} checking $@ ...${ECHO_C}"
  fi
}
creating () { echo ${ECHO_N} "${stars} creating $@ ...${ECHO_C}"; }
message () { echo "${stars} $@"; }
result () { echo "${ECHO_T} $@"; }

## Get one field including all continuation lines from a DCF file
get_dcf_field () {
  ws="[ 	]"		# space and tab
  sed -n "/^${1}:/,/^[^ ]/{p;}" ${2} | \
    sed -n "/^${1}:/{s/^${1}:${ws}*//;p;}
            /^${ws}/{s/^${ws}*//;p;}"
}

## For updating `INDEX' and `data/00Index'
updateIndex () {
  oldindex=${1}
  newindex=${TEMP:-/tmp}/Rbuild${$}
  ${R_HOME}/bin/Rdindex.bat ${Rdfiles} > ${newindex}
  if diff -b ${newindex} ${oldindex} > /dev/null; then
    result "OK"
  else
    result "NO"
    if ${force}; then
      echo "overwriting \`${oldindex}' as \`--force' was given"
      cp ${newindex} ${oldindex}
    else
      echo "use \`--force' to overwrite the existing \`${oldindex}'"
    fi
  fi
  rm -f ${newindex}
}

## For updating `TITLE'
updateTitle () {
  oldtitle=TITLE
  newtitle=${TEMP:-/tmp}/Rbuild${$}
  gawk -f ${R_HOME}/src/gnuwin32/maketitle DESCRIPTION > ${newtitle}
  foo=`cat TITLE`
  package=`set - ${foo}; echo ${1}`
  title=`set - ${foo}; shift; echo ${*}`
  if test "${package}" != "${1}"; then
    if ${force}; then
      result "NO"
      echo "overwriting \`${oldtitle} as \`--force' was given"
      cp ${newtitle} ${oldtitle}
    else
      result "ERROR: package names do not match"
      echo "use \`--force' to overwrite the existing \`${oldtitle}'"
      exit 1
    fi
  elif diff -b ${newtitle} ${oldtitle} > /dev/null; then
    result "OK"
  else
    result "NO"
    if ${force}; then
      echo "overwriting \`${oldtitle} as \`--force' was given"
      cp ${newtitle} ${oldtitle}
    else
      echo "use \`--force' to overwrite the existing \`${oldtitle}'"
    fi
  fi
  rm -f ${newtitle}
}

## Initial checks
checkdir () {
  cd ${start_dir}

  checking "package dir"
  if test -d ${1}; then
    dir=`cd ${1}; pwd`
  else
    result "ERROR: package dir \`${1}' does not exist"
    exit 1
  fi
  result "OK"

  ## Check whether `DESCRIPTION' exists and contains required fields
  checking "for \`DESCRIPTION'"
  if test -r ${dir}/DESCRIPTION; then
    :
  else
    result "ERROR: file \`DESCRIPTION' not found"
    exit 1
  fi
  result "OK"
  checking "\`DESCRIPTION' license entry"
  license=`get_dcf_field License ${dir}/DESCRIPTION`
  if test -z "${license}"; then
    result "ERROR: no license entry in \`DESCRIPTION'"
    exit 1
  fi
  result "OK"
  checking "\`DESCRIPTION' version entry"
  foo=`grep "^Version" ${dir}/DESCRIPTION`
  version=`set - ${foo}; echo ${2}`
  if test -z "${version}"; then
    result "ERROR: no version entry in \`DESCRIPTION'"
    exit 1
  fi
  result "OK"
}

## The work horse
checkpkg () {
  cd ${start_dir}

  if ${is_bundle}; then
    ## If this package is part of a bundle, we need to make sure the
    ## directory exists and contains a `DESCRIPTION.in' file.
    ## Note that this code repeats the first two tests in checkdir().
    checking "package dir"
    if test -d ${1}; then
      dir=`cd ${1}; pwd`
    else
      result "ERROR: package dir \`${1}' does not exist"
      exit 1
    fi
    result "OK"
    description=DESCRIPTION.in
    checking "for \`${description}'"
    if test -r ${dir}/${description}; then
      :
    else
      result "ERROR: file \`${description}' not found"
      exit 1
    fi
    result "OK"
  else
    dir=`cd ${1}; pwd`
    description=DESCRIPTION
  fi

  cd ${dir}

  checking "\`${description}' package entry"
  package=`get_dcf_field Package ${description}`
  if test -z "${package}"; then
    result "ERROR: no package entry in \`${description}'"
    exit 1
  fi
  if test "${package}" != `basename ${dir}`; then
    result "ERROR: package field in \`${description}' differs from dir name"
    exit 1
  fi
  result "OK"

  ## Check for title entry and/or `TITLE'
  checking "\`${description}' title entry"
  title=`get_dcf_field Title ${description}`
  if test -z "${title}"; then
    result "WARNING: no title entry in \`${description}'"
  else
    result "OK"
  fi
  checking "for \`TITLE'"
  if test -r TITLE; then
    result "OK"
    if test -n "${title}"; then
      checking "whether \`TITLE' is up-to-date"
      updateTitle "${package}"
    fi
  else
    result "NO"
    if test -n "${title}"; then
      creating "\`TITLE' from \`${description}' title entry"
      ${R_HOME}/bin/maketitle > TITLE
      if test ${?} -ne 0; then
	result "ERROR"
	exit 1
      else
	result "OK"
      fi
    else
      result "ERROR: no \`TITLE' and no \`${description}' title entry"
      exit 1
    fi
  fi

  ## Check R documentation files
  if test -d man; then
    checking "Rd files"
    all_file=${TEMP:-/tmp}/Rall${$}
    tag_file=${TEMP:-/tmp}/Rtag${$}
    rm -f ${all_file} 2>/dev/null
    LC_ALL=C find man -name "*.[Rr]d" -print | sort > ${all_file}
    Rdfiles=`cat ${all_file}`
    if test -n "${Rdfiles}"; then
      badfiles=`grep "<" ${all_file}`
      if test -n "${badfiles}"; then
	result "ERROR"
	echo "Cannot handle Rd file names containing \`<'."
	echo "These are not legal file names on all R platforms."
	echo "Please rename the following files and try again."
	echo "${badfiles}"
	exit 1
      fi
      any=
      for tag in name alias title description keyword; do
	rm -f ${tag_file} 2>/dev/null
	grep -l "^\\\\${tag}" ${Rdfiles} > ${tag_file} 2>/dev/null
	badfiles=`comm -2 -3 ${all_file} ${tag_file}`
	if test -n "${badfiles}"; then
	  if test -z "${any}"; then
	    result "WARNING:"
	  fi
	  any="${any} ${tag}"
	  echo "Rd files without \`${tag}':"
	  echo "${badfiles}"
	fi
      done
      for tag in name title description usage arguments format details \
	         value references source seealso examples note author  \
	         synopsis; do
	rm -f ${tag_file} 2>/dev/null
	pattern="^\\\\${tag}"
	for f in ${Rdfiles}; do
	  count=`grep ${pattern} ${f} | wc -l`
	  if test ${count} -gt 1; then
	    echo ${f} >> ${tag_file}
	  fi
	done
	if test -f ${tag_file}; then
	  if test -z "${any}"; then
	    result "WARNING:"
	  fi
	  any="${any} ${tag}"
	  echo "Rd files with duplicate \`${tag}':"
	  cat ${tag_file}
	fi
      done
      if test -z "${any}"; then
	result "OK"
      fi
    else
      result "WARNING: no Rd files found"
    fi
    rm -f ${all_file} ${tag_file} 2>/dev/null
  fi

  Rdfiles=`echo "${Rdfiles}" | sed -e '/unix\//d'`

  ## Check for `INDEX'
  checking "for \`INDEX'"
  if test -r INDEX; then
    result "OK"
    if test -n "${Rdfiles}"; then
      checking "whether \`INDEX' is up-to-date"
      updateIndex "INDEX"
    fi
  else
    result "NO"
    if test -n "${Rdfiles}"; then
      creating "\`INDEX' from Rd files"
      perl ${R_HOME}/bin/Rdindex.bat ${Rdfiles} > INDEX
      if test ${?} -ne 0; then
	result "ERROR"
	exit 1
      else
	result "OK"
      fi
    else
      result "ERROR: no \`INDEX' and no Rd files found."
      exit 1
    fi
  fi

  ## Update `data/00Index'
  if test -d data; then
    if test -n "${Rdfiles}"; then
      Rdfiles=`grep -l "\\\\keyword{datasets}" ${Rdfiles}`
    fi
    checking "for \`data/00Index'"
    if test -r data/00Index; then
      result "OK"
      if test -n "${Rdfiles}"; then
	checking "whether \`data/00Index' is up-to-date"
	updateIndex "data/00Index"
      fi
    else
      result "NO"
      if test -n "${Rdfiles}"; then
	creating "\`data/00Index' from Rd files"
	perl ${R_HOME}/bin/Rdindex.bat ${Rdfiles} > data/00Index
	if test ${?} -ne 0; then
	  result "ERROR"
	  exit 1
	else
	  result "OK"
	fi
      else
	result "ERROR: no \`data/00Index' and no Rd files found."
	exit 1
      fi
    fi
  fi

  ## Check for undocumented objects
  if test -d R -a -d man; then
    checking "for undocumented objects"
    Rcmd=${TEMP:-/tmp}/Rcmd${$}
    Rout=${TEMP:-/tmp}/Rout${$}
    echo "undoc(dir = \"${dir}\")" > ${Rcmd}
    ${R_HOME}/bin/Rterm.exe ${R_opts} < ${Rcmd} > ${Rout}
    rm ${Rcmd}
    err=`grep "^Error" ${Rout}`
    out=`grep "^ *\\[" ${Rout}`
    rm ${Rout}
    if test -z "${err}"; then
      if test -n "${out}"; then
	result "WARNING:"
	echo "${out}"
      else
	result "OK"
      fi
    else
      err=`echo "${err}" | sed -e 's/^Error *//'`
      result "ERROR:"
      echo "${err}"
      exit 1
    fi
  fi

  ## Clean up `src'
  if test -d src; then
    message "cleaning \`src'"
    if test -r src/Makefile; then
      (cd src && make clean)
    else
      rm -f src/*.o src/*.s[lo]
    fi
  fi

  ## Other cleanups
  if test -x cleanup; then
    message "running \`cleanup'"
    ./cleanup
  fi
}

buildpkg () {
  cd ${start_dir}/${1}
  ## Final cleanups
  for f in .Rdata .Rhistory; do
    junk=`find . -name ${f}`
    if test -n "${junk}"; then
      message "removing left-over \`${f}' files"
      rm -f ${junk}
    fi
  done
  ## And finally build the package (bundle)
  cd ..
  package=`basename ${1}`
  exclude_file=${TEMP:-/tmp}/Rbuild${$}
  rm -f ${exclude_file} 2>/dev/null \
    || (echo "cannot create file with exclude patterns" && exit 2)
  find ${package} -type d -name check >> ${exclude_file}
  find ${package} -type d -name '*[Oo]ld' >> ${exclude_file}
  find ${package} -name GNUMakefile >> ${exclude_file}
  find ${package} -name CVS >> ${exclude_file}
  find ${package} -name \*~ >> ${exclude_file}
  message "building \`${start_dir}/${package}_${version}.tar.gz'"
  tar chfX - ${exclude_file} ${package} | \
    gzip -c9 > ${start_dir}/${package}_${version}.tar.gz
  rm -f ${exclude_file}
}

## The main loop
if ${debug}; then set -x; fi
for p in ${pkgs}; do
  echo "Building package \`${p}'"
  checkdir ${p}
  if grep "^Contains" ${p}/DESCRIPTION >/dev/null; then
    echo "Looks like \`${p}' is a package bundle"
    is_bundle=true
    bundlepkgs=`get_dcf_field Contains ${p}/DESCRIPTION`
    for pp in ${bundlepkgs}; do
      message "checking package \`${pp}' in bundle \`${p}'"
      stars="**"
      checkpkg ${p}/${pp}
      stars="*"
    done
  else
    is_bundle=false
    checkpkg ${p}
  fi
  buildpkg ${p}
  echo
done

### Local Variables: ***
### mode: sh ***
### sh-indentation: 2 ***
### End: ***
