out=$1
shift
for rdfile in $*; do \
    echo "% --- Source file: ${rdfile} ---";
    cat ${rdfile}; echo; echo '\eof';
done > $out
