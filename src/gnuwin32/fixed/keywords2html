#-*- perl -*-
#
# Translate KEYWORDS.db to HTML
#

sub searchKey
{
    my $key=$_[0];
    my $retval = "<a href=\"";
    $retval .= "javascript:searchInIndex('$key', false, true, false);";
    $retval .= "\">$key</a>";
    return($retval);
}

@key = ();
while(<>){
    s/#.*//;
    chop;
    push(@key, $_) if $_;
}

@key = sort(@key);

$depth=0;
print("<dl>\n");
for($k=0;$k<=$#key; $k++){

    ($keywords, $desc) = split(':', $key[$k]);
    @keywords = split('\|', $keywords);

    if($#keywords==0){
	for($n=0; $n<$depth-$#keywords; $n++){
	    print "</ul>";
	}
	print "</dd><p>\n" if($depth);
	print "<dt> $desc</dt>\n<dd>\n";
    }
    else{
	for($n=0; $n<$#keywords ; $n++){
	    print " ";
	}
	if($#keywords > $depth){
	    print "<ul><li>";
	    print searchKey($keywords[$#keywords]);
	    print ": $desc</li>\n";
	}
	elsif($#keywords < $depth){
	    for($n=0; $n<$depth-$#keywords; $n++){
		print "</ul>";
	    }
	    print "\n";
	    print "<li>", searchKey($keywords[$#keywords]);
	    print ": $desc</li>\n";
	}
	else{
	    print "<li>", searchKey($keywords[$#keywords]);
	    print ": $desc</li>\n";
	}
    }
    $depth=$#keywords;
}	
for($n=0; $n<$depth; $n++){
    print "</ul>";
}
print "</dd></dl>\n";
	
