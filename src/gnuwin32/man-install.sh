out=$1
shift
echo $*
for rdfile in $*; do cat ${rdfile}; echo; echo '\eof'; done > $out
