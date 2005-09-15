#
##  Rd2dvi.sh -- Convert man pages (*.Rd help files) via LaTeX to DVI/PDF.
##
## Examples:
##  R CMD Rd2dvi.sh /path/to/Rsrc/src/library/base/man/Normal.Rd
##  R CMD Rd2dvi.sh `grep -l "\\keyword{distr" \
##                  /path/to/Rsrc/src/library/stats/man/*.Rd | sort | uniq`

## This is set by R CMD on Unix but not on Windows
R_PAPERSIZE=${R_PAPERSIZE-a4}

revision='$Revision: 1.41 $'
version=`set - ${revision}; echo ${2}`
version="Rd2dvi.sh ${version}

Copyright (C) 2000-2005 The R Core Development Team.
This is free software; see the GNU General Public Licence version 2
or later for copying conditions.  There is NO warranty." 

usage="Usage: R CMD Rd2dvi [options] files

Generate DVI (or PDF) output from the Rd sources specified by files, by
either giving the paths to the files, or the path to a directory with
the sources of a package.

Unless specified via option '--output', the basename of the output file
equals the basename of argument 'files' if this specifies a package
(bundle) or a single file, and 'Rd2' otherwise.

Options:
  -h, --help		print short help message and exit
  -v, --version		print Rd2dvi version info and exit
      --batch		no interaction
      --debug		turn on shell debugging (set -x)
      --no-clean	do not remove created temporary files
      --no-preview	do not preview generated output file
      --os=NAME		use OS subdir 'NAME' (unix or windows)
      --OS=NAME		the same as '--os'
  -o, --output=FILE	write output to FILE
      --pdf		generate PDF output
      --title=NAME	use NAME as the title of the document
  -V, --verbose		report on what is done

Report bugs to <r-bugs@r-project.org>."

TEXINPUTS=.\;${R_HOME}/share/texmf\;${TEXINPUTS}
export TEXINPUTS

start_dir=`pwd`

batch=false
clean=true
debug=false
out_ext="dvi"
output=""
preview=${xdvi-xdvi.bat}
verbose=false
OSdir=windows

while test -n "${1}"; do
  case ${1} in
    -h|--help)
      echo "${usage}"; exit 0 ;;
    -v|--version)
      echo "${version}"; exit 0 ;;
    --batch)
      batch=true ;;
    --debug)
      debug=true ;;
    --no-clean)
      clean=false ;;
    --no-preview)
      preview=false ;;
    --pdf)
      out_ext="pdf";
      preview=false;
      R_RD4DVI=${R_RD4PDF-"times,hyper"};
      R_LATEXCMD=${PDFLATEX-pdflatex} ;;
    --title=*)
      title=`echo "${1}" | sed -e 's/[^=]*=//'` ;;
    -o)
      if test -n "`echo ${2} | sed 's/^-.*//'`"; then      
	output="${2}"; shift
      else
	echo "ERROR: option '${1}' requires an argument"
	exit 1
      fi
      ;;
    --output=*)
      output=`echo "${1}" | sed -e 's/[^=]*=//'` ;;
    --OS=*|--os=*)
      OSdir=`echo "${1}" | sed -e 's/[^=]*=//'` ;;
    -V|--verbose)
      verbose=echo ;;
    --|*)
      break ;;
  esac
  shift
done

if ${debug}; then set -x; fi

get_dcf_field () {
  ## Get one field including all continuation lines from a DCF file.
  ws="[ 	]"              # space and tab
  sed -n "/^${1}:/,/^[^ 	]/{p;}" ${2} | \
    sed -n "/^${1}:/{s/^${1}:${ws}*//;p;}
            /^${ws}/{s/^${ws}*//;p;}"
}

Rdconv_dir_or_files_to_LaTeX () {
  ## Convert Rd files in a dir or a list of Rd files to LaTeX, appending
  ## the result to OUTFILE.
  ## Usage:
  ##   Rdconv_dir_or_files_to_LaTeX OUTFILE DIR
  ##   Rdconv_dir_or_files_to_LaTeX OUTFILE FILES

  ${verbose} $@

  out="${1}"
  shift

  if test -d ${1}; then
    files=`ls ${1}/*.[Rr]d`
    if test -d ${1}/${OSdir}; then
      files="${files} `ls ${1}/${OSdir}/*.[Rr]d`"
    fi
    files=`LC_ALL=C echo ${files} | sort`
  else
    files="${@}"
  fi

  echo "Converting Rd files to LaTeX ..."

  for f in ${files}; do
    echo ${f}
    ${R_CMD} Rdconv -t latex ${f} >> ${out}
  done

}

Rd_DESCRIPTION_to_LaTeX () {
  ## Typeset the contents of a DESCRIPTION file in a LaTeX description
  ## list.
  ## Usage:
  ##   Rd_DESCRIPTION_to_LaTeX FILE
  
  fields=`sed '/^[ 	]/d; s/^\([^:]*\):.*$/\1/' $1`
  echo "\\begin{description}"
  echo "\\raggedright{}"
  for f in `echo "${fields}" | sed '/Package/d; /Bundle/d;'`; do
    text=`get_dcf_field ${f} ${1}`
    echo "\\item[${f}] \\AsIs{${text}}"
  done
  echo "\\end{description}"
}

is_bundle=no
is_base_package=no
file_sed='s/[_$]/\\&/g'

toc="\\Rdcontents{\\R{} topics documented:}"
if test -d "${1}"; then
  if test -f ${1}/DESCRIPTION; then
    if test -n "`grep '^Bundle:' ${1}/DESCRIPTION`"; then
      echo "Hmm ... looks like a package bundle"
      is_bundle=yes
      bundle_name=`get_dcf_field Bundle "${1}/DESCRIPTION"`
      bundle_pkgs=`get_dcf_field Contains "${1}/DESCRIPTION"`
      title=${title-"Bundle \`${bundle_name}'"}
    else
      echo "Hmm ... looks like a package"
      package_name=`get_dcf_field Package "${1}/DESCRIPTION"`
      title=${title-"Package \`${package_name}'"}
      dir=${1}/man
    fi
    test -z "${output}" && output="`basename ${1}`.${out_ext}"
  elif test -f ${1}/DESCRIPTION.in && \
       test -n "`grep '^Priority: *base' ${1}/DESCRIPTION.in`"; then
    is_base_package=yes
    echo "Hmm ... looks like a package from the R distribution"
    package_name=`get_dcf_field Package "${1}/DESCRIPTION.in"`
    title=${title-"Package \`${package_name}'"}
    dir=${1}/man
    test -z "${output}" && output="`basename ${1}`.${out_ext}"    
  else
    if test -d ${1}/man; then
      dir=${1}/man
    else
      dir=${1}
    fi
    subj0=`echo ${dir} | sed -e ${file_sed}`
    subj="all in \\file{${subj0}}"
  fi
else
  if test ${#} -gt 1 ; then
    subj=" etc.";
  else
    subj=
    toc=
    if test -z "${output}"; then
      output=`basename "${1}"`
      output="`echo ${output} | sed 's/[Rr]d$//'`${out_ext}"
    fi
  fi
  subj0=`echo ${1} | sed -e ${file_sed}`
  subj="\\file{${subj0}}${subj}"
fi
## substitution went wrong under ash
title1="\\R{} documentation}} \\par\\bigskip{{\\Large of ${subj}"
title=${title-$title1}

## Prepare for building the documentation.
if test -z "${output}"; then
  output="Rd2.${out_ext}"
fi
if test -f ${output}; then
  echo "file '${output}' exists; please remove first"
  exit 1
fi
# pid is always 1000 on Windows sh.exe
#build_dir=.Rd2dvi${$}
build_dir=.Rd2dvi
if test -d ${build_dir}; then
  rm -rf ${build_dir} || echo "cannot write to build dir" && exit 2
fi
mkdir ${build_dir}

## Rd2.tex part 1: header
if test ${batch}; then
  cat > ${build_dir}/Rd2.tex <<EOF
\\nonstopmode{}
EOF
else
  cat > ${build_dir}/Rd2.tex <<EOF
EOF
fi
cat >> ${build_dir}/Rd2.tex <<EOF
\\documentclass[${R_PAPERSIZE}paper]{book}
\\usepackage[${R_RD4DVI-ae}]{Rd}
\\usepackage{makeidx}
\\usepackage[@ENC@]{inputenc}
\\makeindex{}
\\begin{document}
EOF
if test "${is_bundle}" = no; then
  cat >> ${build_dir}/Rd2.tex <<EOF
\\chapter*{}
\\begin{center}
{\\textbf{\\huge ${title}}}
\\par\\bigskip{\\large \\today}
\\end{center}
EOF
  if test -f ${1}/DESCRIPTION; then
    Rd_DESCRIPTION_to_LaTeX ${1}/DESCRIPTION >> ${build_dir}/Rd2.tex
  fi
  if test "${is_base_package}" = yes; then
    R_version=unknown
    if test -f ${1}/../../../VERSION; then
      R_version=`cat ${1}/../../../VERSION`
    fi
    Rd_DESCRIPTION_to_LaTeX ${1}/DESCRIPTION.in | \
      sed "s/@VERSION@/${R_version}/" >> ${build_dir}/Rd2.tex
  fi
else
  cat >> ${build_dir}/Rd2.tex <<EOF
\\pagenumbering{Roman}
\\begin{titlepage}
\\strut\\vfill
\\begin{center}
{\\textbf{\\Huge ${title}}}
\\par\\bigskip{\\large \\today}
\\end{center}
\\par\\bigskip
EOF
  Rd_DESCRIPTION_to_LaTeX ${1}/DESCRIPTION >> ${build_dir}/Rd2.tex
  cat >> ${build_dir}/Rd2.tex <<EOF
\\vfill\\vfill
\\end{titlepage}
EOF
fi
  
## Rd2.tex part 2: body
if test "${is_bundle}" = no; then
  echo ${toc} >> ${build_dir}/Rd2.tex
  Rdconv_dir_or_files_to_LaTeX ${build_dir}/Rd2.tex ${dir-${@}}
else
  cat >> ${build_dir}/Rd2.tex <<EOF
\\setcounter{secnumdepth}{-1}
\\pagenumbering{roman}
\\tableofcontents{}
\\cleardoublepage{}
\\pagenumbering{arabic}
EOF
  for p in ${bundle_pkgs}; do
    echo "Bundle package: '${p}'"
    echo "\\chapter{Package \`${p}'}" >> ${build_dir}/Rd2.tex
    if test -f ${1}/${p}/DESCRIPTION.in; then
      Rd_DESCRIPTION_to_LaTeX ${1}/${p}/DESCRIPTION.in \
        >> ${build_dir}/Rd2.tex
    fi
    Rdconv_dir_or_files_to_LaTeX ${build_dir}/Rd2.tex ${1}/${p}/man
    echo "\\clearpage{}" >> ${build_dir}/Rd2.tex
  done
  echo "\\cleardoublepage{}" >> ${build_dir}/Rd2.tex
fi

## Rd2.tex part 3: footer
cat >> ${build_dir}/Rd2.tex <<EOF
\\printindex{}
\\end{document}
EOF

## Look for encodings
ENCS=`grep '^\\\\inputencoding' ${build_dir}/Rd2.tex | uniq |\
  sed -e 's/^\\\\inputencoding{\(.*\)}/\1/'`
ENCS=`grep '^\\\\\inputencoding' ${build_dir}/Rd2.tex |  uniq | \
  sed -e 's/^\\\\inputencoding{\(.*\)}/\1/' | \
  tr '\na-z0-9' ',a-z0-9' | sed -e s/,$//`
echo "ENCS is ${ENCS}"

## substitute for the encodings used
mv ${build_dir}/Rd2.tex ${build_dir}/Rd2.tex.pre
if test -z "${ENCS}"; then
  sed -e '/^\\usepackage\[@ENC@\]{inputenc}$/d' \
    ${build_dir}/Rd2.tex.pre > ${build_dir}/Rd2.tex
else
  sed -e s/^\\\\usepackage\\[@ENC@\\]/\\\\usepackage[${ENCS}]/ \
    ${build_dir}/Rd2.tex.pre > ${build_dir}/Rd2.tex
fi

## <FIXME>
## Need to do something smarter about the exit status in batch mode.
status=0
## <FIXME>

miktex=`latex --version | grep ^MiKTeX | wc -l`
if test "${miktex}" eq "1"; then
R_TEXOPTS=--include-directory=$(RHOME)/share/texmf
else
R_TEXOPTS=
fi

echo "Creating ${out_ext} output from LaTeX ..."
cd ${build_dir}
${R_LATEXCMD-latex} ${R_TEXOPTS} Rd2 || status=1
${R_MAKEINDEXCMD-makeindex} Rd2
${R_LATEXCMD-latex} ${R_TEXOPTS} Rd2
if test "${out_ext}" = pdf; then
  ${R_LATEXCMD-latex} ${R_TEXOPTS} Rd2
fi
cd ${start_dir}
echo "Saving output to '${output}' ..."
cp ${build_dir}/Rd2.${out_ext} ${output}
echo "Done"

if ${clean}; then
  rm -rf ${build_dir}
else
  echo "You may want to clean up by 'rm -rf ${build_dir}'"
fi
${preview} ${output}
exit ${status}

### Local Variables: ***
### mode: sh ***
### sh-indentation: 2 ***
### End: ***
