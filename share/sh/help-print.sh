#! /bin/sh

## Usage is
##   sh help-print.sh FILE TOPIC LATEX DVIPS
## see src/library/utils/R/unix/help.R.

FILE="${1}"
TOPIC="${2}"
LATEX="${3}"
if test "${LATEX}" = "false"; then
    echo "Cannot print, latex seems to be unavailable"
    exit 1
fi
shift 3
DVIPS="${@}"
if test "${DVIPS}" = "false"; then
    echo "Cannot print, dvips seems to be unavailable"
    exit 2
fi
ODIR=`pwd`
cd `(dirname "${FILE}") 2>/dev/null || \
     echo "${FILE}" | sed -e 's,[^/]*$,,;s,/$,,;s,^$,.,'`
${LATEX} "\\nonstopmode\\input{${FILE}}" >/dev/null 2>&1
${DVIPS} "${FILE}" 2>/dev/null
if test -f "${FILE}.ps"; then
    echo "Saving help page to '${TOPIC}.ps'"
    mv "${FILE}.ps" "${ODIR}/${TOPIC}.ps"
fi
rm -f "${FILE}.aux" "${FILE}.dvi" "${FILE}.log"

### Local Variables: ***
### mode: sh ***
### sh-indentation: 2 ***
### End: ***
