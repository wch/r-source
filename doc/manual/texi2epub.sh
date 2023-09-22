#!/bin/sh

## Generate EPUB for the R manuals via texi2any --epub (needs texi2any 7
## or later) and post-process results to give valid EPUB according to
## epubcheck, provinding that zip and unzip are available.
## Arguments:
##   maj min texi html
## with maj and min the maj and min components of the tex2any version,
## texi the path to the Texinfo source and html the name of the EPUB
## target.

SED=${SED-sed}
TEXI2ANY=${TEXI2ANY-texi2any}
UNZIPCMD=${UNZIPCMD-unzip}
ZIPCMD=${ZIPCMD-zip}

## Avoid dirname for portability ... really needed?
srcdir=`echo "$0" | ${SED} 's/\/texi2epub.sh//'`

${TEXI2ANY} --epub -I${srcdir} $3 -o $4
if test -n "${UNZIPCMD}" && test -n "${ZIPCMD}"; then
  mv $4 $4.zip
  mkdir $4.dir && cd $4.dir
  ${UNZIPCMD} -q ../$4.zip
  ## Apparenty, for EPUB 3.2 (but not 3.3?) one needs something like
  ##   <meta property="dcterms:modified">2011-01-01T12:00:00Z</meta>
  date=`date +%Y-%m-%dT%TZ`
  ## Should be portable according to
  ## <https://pubs.opengroup.org/onlinepubs/9699919799/utilities/date.html>
  cat >fix1.sed <<-EOF
	/<meta property="dcterms:modified">/d
	s/^\( *\)\(<dc:title>\)/\1<meta property="dcterms:modified">${date}<\/meta>\\
	\1\2/
EOF
  for f in EPUB/*.opf; do
    mv $f $f.tmp
    ${SED} -f fix1.sed $f.tmp > $f
    rm -f $f.tmp
  done
  cat >fix2.sed <<-EOF
	s/\(<table[^>]*\)border="0">/\1border="">/
	s/<td width="/<td style="width: /g
	s/<th width="/<th style="width: /g
	s/\(<dd><a class="index-entry-id" id="index-anova">\)/<dt><\/dt>\1/
EOF
  for f in EPUB/xhtml/*.xhtml; do
    mv $f $f.tmp
    ${SED} -f fix2.sed $f.tmp > $f
    rm -f $f.tmp
  done
  ## Apparently extra fields are not allowed in mimetype.
  ${ZIPCMD} -q -X ../$4 mimetype
  ${ZIPCMD} -q -r ../$4 META-INF EPUB
  cd .. 
  rm -rf $4.dir $4.zip
fi

