out=$1
shift
for rdfile in $*; do cat ${rdfile}; echo; echo '\eof'; done > $out
