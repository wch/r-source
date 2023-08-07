/^<meta/s/charset=iso-8859-1/charset=UTF-8/
s/`\([^`']*\)'/‘\1’/g
s/ '\([^`']*\)' / ‘\1’ /g
s/ '\([^`']*\)'$/ ‘\1’/g
s/ '\([^`']*\)'\([,:;!\?.>\)]\)/ ‘\1’\2/g
s/^'\([^`']*\)' /‘\1’ /g

# obsolete for texi2any >= 7.0 with `-c IGNORE_REF_TO_TOP_NODE_UP=1`
/<link href="dir.html#Top"/d
s/, \(Previous\|Up\): <a href="dir.html#Top"[^\/]*\/a>//g

s/<table/<table summary=""/
s/<meta name="desc/<meta name="viewport" content="width=device-width, initial-scale=1.0">\
<meta name="desc/
