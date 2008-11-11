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
status=0
cd `(dirname "${FILE}") 2>/dev/null || \
     echo "${FILE}" | ${SED=sed} -e 's,[^/]*$,,;s,/$,,;s,^$,.,'`
${LATEX} "\\nonstopmode\\input{${FILE}}" >/dev/null 2>&1  || status=1
if test $status -gt 0; then
  echo "Error in running latex command ('${LATEX}')"
  exit 1
fi
${DVIPS} "${FILE}" 2>/dev/null || status=1
if test $status -gt 0; then
  echo "Error in running dvips command ('${DVIPS}')"
  exit 1
fi
if test -f "${FILE}.ps"; then
  echo "Saving help page to '${TOPIC}.ps'"
  mv "${FILE}.ps" "${ODIR}/${TOPIC}.ps"
else
  echo "Help page sent to printer" 
fi
rm -f "${FILE}.aux" "${FILE}.dvi" "${FILE}.log"

### Local Variables: ***
### mode: sh ***
### sh-indentation: 2 ***
### End: ***
