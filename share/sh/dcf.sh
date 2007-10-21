get_dcf_field () {
  ## Get one field including all continuation lines from a DCF file.
  ## Usage:
  ##   get_dcf_field FIELD FILE
  ws="[ 	]"		# space and tab
  ${SED-sed} -n "/^${1}:/,/^[^ ]/p" ${2} | \
    ${SED-sed} -n "1s/^${1}:${ws}*//p; /^${ws}/p" | \
    ${SED-sed} "/^${ws}/s/^${ws}*//; s/[ 	]*$//"
    ## (Strip leading tag [first match], delete all additional tag
    ## lines, and remove leading whitespace from continuation lines.)
}

### Local Variables: ***
### mode: sh ***
### sh-indentation: 2 ***
### End: ***
