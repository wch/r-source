/^<meta/s/charset=iso-8859-1/charset=UTF-8/
s/`\([^`']*\)'/‘\1’/g
s/ '\([^`']*\)' / ‘\1’ /g
s/ '\([^`']*\)'$/ ‘\1’/g
s/ '\([^`']*\)'\([,:;!\?.>\)]\)/ ‘\1’\2/g
s/^'\([^`']*\)' /‘\1’ /g
/<link href="dir.html#Top"/d
s/, \(Previous\|Up\): <a href="dir.html#Top"[^\/]*\/a>//g
s/<table/<table summary=""/
