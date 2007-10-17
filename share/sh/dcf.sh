get_dcf_field () {
  ## Get one field including all continuation lines from a DCF file.
  ## Usage:
  ##   get_dcf_field FIELD FILE
  ws="[ 	]"		# space and tab
  sed -n "/^${1}:/,/^[^ ]/{p;}" ${2} | \
    sed "1,1{s/^${1}:${ws}*//;};
         /^${1}:/,/^[^ ]/{d;};
         /^${ws}/{s/^${ws}*//};
         s/[ 	]*$//"
}

### Local Variables: ***
### mode: sh ***
### sh-indentation: 2 ***
### End: ***
