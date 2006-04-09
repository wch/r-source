#! /bin/sh

: ${R_DOC_DIR}="${R_HOME}/doc"
USER_R_HOME="${1}/.R"; shift
rm -rf "${USER_R_HOME}"

for d in "${USER_R_HOME}" "${USER_R_HOME}/doc" \
         "${USER_R_HOME}/doc/html" "${USER_R_HOME}/doc/html/search" \
	 "${USER_R_HOME}/library"; do \
  "${R_HOME}"/bin/mkinstalldirs "${d}" >/dev/null
done

for f in AUTHORS COPYING THANKS; do
  if test -f "${R_DOC_DIR}/${f}"; then
    ${LN_S} "${R_DOC_DIR}/${f}" "${USER_R_HOME}/doc/${f}"
  fi
done

if test -d "${R_DOC_DIR}/manual"; then
  ${LN_S} "${R_DOC_DIR}/manual" "${USER_R_HOME}/doc/manual"
fi

for f in "${R_DOC_DIR}"/html/*.html "${R_DOC_DIR}"/html/*.jpg "${R_DOC_DIR}"/html/R.css ; do
  if test -f "${f}"; then
    ${LN_S} "${f}" "${USER_R_HOME}/doc/html"
  fi
done
## we are going to recreate this in R code
rm -f "${USER_R_HOME}/doc/html/packages.html"
## this needs to be copied for OS X 
rm -f "${USER_R_HOME}/doc/html/index.html"
cp "${R_DOC_DIR}/html/index.html" "${USER_R_HOME}/doc/html"

## class files must be copied for Mozilla to work
for f in "${R_DOC_DIR}"/html/search/*.class; do
  if test -f "${f}"; then
    cp "${f}" "${USER_R_HOME}/doc/html/search"
  fi
done
for f in "${R_DOC_DIR}"/html/search/*.html; do
  if test -f "${f}"; then
    ${LN_S} "${f}" "${USER_R_HOME}/doc/html/search"
  fi
done
## we are going to recreate this in R code
rm -f "${USER_R_HOME}/doc/html/search/index.txt"

${LN_S} "${R_DOC_DIR}/html/R.css" "${USER_R_HOME}/library"

### Local Variables: ***
### mode: sh ***
### sh-indentation: 2 ***
### End: ***
