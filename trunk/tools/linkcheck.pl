#-*- perl -*-

##
## Comment out links that are not available. Used, e.g., for including
## the manuals in the top HTML help page. Works only for lines
## containing only the link tag.
##

while(<>){
    if(/^(\s*)<a href=\"(.*)\">(.*)<\/a>\s$/i){
	if(-f $2){
	    print "$1<a href=\"$2\">$3</a>\n";
	}
	else{
	    $link = $2;
	    $text = $3;
	    if ($link =~ /R-[A-Za-z]+\.html$/) {
		$link =~ s+../manual+http://cran.r-project.org/doc/manuals+;
		print "$1<a href=\"$link\">$text</a> (on CRAN)\n";
	    } else {
		print "$1<!a href=\"$link\">$text (not installed)\n";
	    }
	}
    }
    else{
	print $_;
    }
}
