#! /bin/sh

## Usage is
##   sh help-print.sh FILE topic LATEX DVIPS
## see src/library/base/R/unix/help.R.

LATEX="${3}"
if test "${LATEX}" = "false"; then
    echo "Cannot print, latex seems to be unavailable"
    exit 1
fi
DVIPS="${4}"
if test "${DVIPS}" = "false"; then
    echo "Cannot print, dvips seems to be unavailable"
    exit 2
fi
ODIR=`pwd`
cd `dirname "${1}"`
${LATEX} "\\nonstopmode\\input{${1}}" >/dev/null 2>&1
${DVIPS} ${1} 2>/dev/null
if test -f "${1}.ps"; then
    echo "Saving help page to '${2}.ps'"
    mv "${1}.ps" "${ODIR}/${2}.ps"
fi
rm -f "${1}.aux" "${1}.dvi" "${1}.log"

### Local Variables: ***
### mode: sh ***
### sh-indentation: 2 ***
### End: ***
