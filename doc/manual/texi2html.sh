#!/bin/sh

## For now, only post-process the texi2any --html results.
## Arguments:
##    maj min html
## with maj and min the maj and min components of the tex2any version,
## and html the name of the HTML *target*: the actual input will have
## '.tmp' appended to html.

SED=${SED-sed}
sedscriptuse=$3.sed
if test $1 -ge 7; then
  cat >${sedscriptuse} <<-EOF
	s/<td width="/<td style="width: /g
	s/<th width="/<th style="width: /g
	s/\(<ol class="enumerate"\) type="a"/\1 style="list-style-type: lower-alpha"/
	EOF
elif test "$1" -eq 6 -a "$2" -eq 8; then
  ## drop title (appears duplicated) consistent with Texinfo >= 7.0
  cat >"${sedscriptuse}" <<-EOF
	/<h1 class="settitle"/d
	EOF
fi
${SED} -f ${sedscriptuse} $3.tmp > $3
rm -f ${sedscriptuse}


