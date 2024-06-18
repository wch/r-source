#!/bin/sh

## For now, only post-process the texi2any --html results.
## Could also do the the generation, but options differ according to
## manual (in particular, R-FAQ uses a different CSS).
## Arguments:
##    maj min html
## with maj and min the maj and min components of the tex2any version,
## and html the name of the HTML *target*: the actual input will have
## '.tmp' appended to html.

SED=${SED-sed}
sedscriptini=`echo "$0" | ${SED} 's/texi2html.sh/quot.sed/'`
sedscriptuse=$3.sed
if test $1 -ge 7; then
  cat ${sedscriptini} | ${SED} '/s\/<table/d' > ${sedscriptuse}
  cat >>${sedscriptuse} <<-EOF
	s/<td width="/<td style="width: /g
	s/<th width="/<th style="width: /g
	s/<table summary=""/<table/
	s/\(<ol class="enumerate"\) type="a"/\1 style="list-style-type: lower-alpha"/
	EOF
elif test "$1" -eq 6 -a "$2" -eq 8; then
  ## drop title (appears duplicated) consistent with Texinfo >= 7.0
  cat "${sedscriptini}" - >>"${sedscriptuse}" <<-EOF
	/<h1 class="settitle"/d
	EOF
else
  ## Texinfo < 6.8 lacks the viewport declaration
  cat "${sedscriptini}" - >"${sedscriptuse}" <<-EOF
	s/<meta name="desc/<meta name="viewport" content="width=device-width,initial-scale=1">\\
	<meta name="desc/
	EOF
fi
${SED} -f ${sedscriptuse} $3.tmp > $3
rm -f ${sedscriptuse}


