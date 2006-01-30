out=$1
shift
for rdfile in $*; do
    if echo $rdfile | grep "^man/[A-Za-z0-9]" > /dev/null; then
	echo "% --- Source file: ${rdfile} ---";
	cat ${rdfile}; echo; echo '\eof';
    fi
done > $out
