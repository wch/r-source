# Perl Routines for HTML to RTF
#
# based on hh2rtf by Steve Atkins at
#
# http://www.blighty.com/products/hh2rtf/  which says
#   `Freeware. You may not resell it or claim you wrote it. 
#    You can use it for anything, commercial or otherwise.'
#
# Modifications for R (C) 1999 B. D. Ripley
#
# Routines for HTML to RTF
#
# Steve Atkins, Nov '97

$Font{"P"} = "\\f0\\fs22";
$Font{"H1"} = "\\f0\\fs48\\cf15";
$Font{"H2"} = "\\f0\\fs36\\cf1";
$Font{"H3"} = "\\f0\\fs30\\cf14";
$Font{"H4"} = "\\f0\\fs26";
$Font{"H5"} = "\\f0\\fs20";
$Font{"H6"} = "\\f0\\fs18";
$Font{"PRE"} = "\\f1\\fs20";
$Font{"BLOCKQUOTE"} = "\\f0\\fs20\\li720";

## Routines called by html.pl
$Begin{"HEAD"} = "begin_head";
$End{"HEAD"} = "end_head";

sub begin_head {
    local ($element, $tag) = @_;
    print RTF "#{\\footnote ";
    print RTF "_label_$file}\n";
    print RTF $Font{"P"}, "\n";
    $key = $file;
    $key =~ s/\.html$//o;
    if($key eq "00Index") {$key = "Contents";}
    print RTF "K{\\footnote $key}\n";
    print RTF "+{\\footnote $browse{$key}}\n";

    $ignore_text = 1;}

sub end_head {
    local ($element) = @_;
    $ignore_text = 0;}

$Begin{"TITLE"} = "begin_title";
$End{"TITLE"} = "end_title";

sub begin_title {
    local ($element, $tag, %attributes) = @_;
    $buildtitle = "";
}

sub end_title {
    local ($element) = @_;
    $title = $buildtitle;
    undef $buildtitle;

    print RTF "\${\\footnote $title}\n";
#    print RTF "K{\\footnote $title}\n";
}

$Begin{"BODY"} = "begin_document";

sub begin_document {
    local ($element, $tag) = @_;
}

$End{"BODY"} = "end_document";

sub end_document {
    local ($element) = @_;
}

## Headers

$Begin{"H1"} = "begin_header";
$End{"H1"} = "end_header";

$Begin{"H2"} = "begin_header";
$End{"H2"} = "end_header";

$Begin{"H3"} = "begin_header";
$End{"H3"} = "end_header";

$Begin{"H4"} = "begin_header";
$End{"H4"} = "end_header";

$Begin{"H5"} = "begin_header";
$End{"H5"} = "end_header";

$Begin{"H6"} = "begin_header";
$End{"H6"} = "end_header";

sub begin_header {
    local ($element, $tag, %attributes) = @_;
    print RTF $Font{$element}, " ";
    $inheader = 1;
    if($attributes{"align"} eq "center") {
	print RTF "\\qc "
    }
}

sub end_header {
    local ($element) = @_;
    print RTF "\\sa240\\par\\ql\\pard\\plain\n";
    $inheader = 0;
}

$Begin{"BR"} = "line_break";

sub line_break {
    local ($element, $tag) = @_;
    print RTF "\\line ";
}

$Begin{"P"} = "begin_paragraph";
$End{"P"} = "end_paragraph";

sub begin_paragraph {
    local ($element, $tag, %attributes) = @_;
    print RTF $Font{"P"}, " ";
    if($attributes{"align"} eq "center") {
	print RTF "\\qc "
    }
}

sub end_paragraph {
    local ($element) = @_;
    print RTF "\\sa240\\par\\pard\\plain\n";
}

$Begin{"DIV"} = "begin_division";
$End{"DIV"} = "end_division";

sub begin_division {
    local ($element, $ta, %attributes) = @_;
    print RTF $Font{"P"}, " ";
    if($attributes{"align"} eq "center") {
	print RTF "\\qc "
    }
}

sub end_division {
    local ($element) = @_;
    print RTF "\\sa240\\par\\ql\\pard\\plain\n";
}

$Begin{"A"} = "begin_a";
$End{"A"} = "end_a";

sub begin_a {
    local ($element, $tag, %attributes) = @_;
    $in_a = 1;
    if(exists $attributes{"name"}) { # It's a link target
	$tmp = $attributes{"name"};
	print RTF "#{\\footnote ";
	print RTF "_label_$filename";
	print RTF "_hash_$tmp";
	print RTF "}\n";
    }
    if(exists $attributes{"href"}) { # It's a link source
	$href = $attributes{"href"};
	if(index($href, ":")>=0) {
#	    if(index($href, "JavaScript:hhctrl.TextPopup")<0) {
#		print RTF "{\\cf1\\strike ";
#	    } else {
#		print RTF "{\\ul ";
#	    }
	    undef $href;
	} else {
	    print RTF "{\\strike ";
	}
    } else {
	undef $href;
    }
}

sub end_a {
    local ($element) = @_;
    if(defined $href) {
	if(index($href, "JavaScript") >= 0) {
	    if($href =~ /JavaScript:hhctrl.TextPopup\(\"([^\"]+)\"/) {
		$popup = $1;
		$popup =~ s/&#32;/ /og;
		print STDERR "Popup: $popup\n";
		print RTF "}{\\v _popup_$pcnt} ";
		$popuplist{$pcnt} = $popup;
		$pcnt++;
	    } else {
		print RTF "}{\\v JavaScript_Error}";
	    }
	} else {
	    if(index($href, ":") >= 0) { # It's an absolute URL
		print RTF "\\cf0}{\\v *!EF(`$href',`',1,`');} ";
	    } else { # It's internal
		$hash = index($href, "#");
		if($hash >= 0) { # It's an internal pageref
		    if($hash == 0) {
			$href = $filename . $href;
		    }
		    $href =~ s/#/_hash_/og;
		    print RTF "}{\\v _label_$href}";
		} else {
		    print RTF "}{\\v _label_$href}";
		}
		$href =~ s/_hash_.*//o;
		if(! exists $filehash{$href}) {
		    print STDERR "Reference to file $href not listed in $HHP\n";
		    $missingfiles{$href}++;
		}
	    }
	}
    }
    undef $href; undef $in_a;
}

$Begin{"BLOCKQUOTE"} = "begin_blockquote";
$End{"BLOCKQUOTE"} = "end_blockquote";

sub begin_blockquote {
    local ($element, $tag) = @_;
    print RTF $Font{"BLOCKQUOTE"}, " ";
}

sub end_blockquote {
    local ($element) = @_;
    print RTF "\\sa240\\par\\pard\\plain\n";
}

$Begin{"PRE"} = "begin_pre";
$End{"PRE"} = "end_pre";

sub begin_pre {
    local ($element, $tag) = @_;
    print RTF $Font{"PRE"};
    $whitespace_significant = 1;
}

sub end_pre {
    local ($element) = @_;
    print RTF "\\sa240\\par\\pard\\plain\n";
    $whitespace_significant = 0;
}

$Begin{"INPUT"} = "form_input";

sub form_input {
    local ($element, $tag, *attributes) = @_;
}

$Begin{"HR"} = "horizontal_rule";

sub horizontal_rule {
    local ($element, $tag) = @_;
}

# Add code for IMG (use ALT attribute)
# Ignore I, B, EM, TT, CODE (no font changes)

$Begin{"B"} = "begin_b";
$End{"B"} = "end_b";

$Begin{"I"} = "begin_i";
$End{"I"} = "end_i";

$Begin{"EM"} = "begin_i";
$End{"EM"} = "end_i";

$Begin{"TT"} = "begin_tt";
$End{"TT"} = "end_tt";

$Begin{"CODE"} = "begin_tt";
$End{"CODE"} = "end_tt";

sub begin_i {
    local ($element, $tag) = @_;
    print RTF "\\i ";
}

sub end_i {
    local ($element) = @_;
    print RTF "\\i0 ";
}

sub begin_b {
    local ($element, $tag) = @_;
    print RTF "\\b ";
}

sub end_b {
    local ($element) = @_;
    print RTF "\\b0 ";
}

sub begin_tt {
    local ($element, $tag) = @_;
    if($inheader > 0) {
	print RTF "\\f1\\b", " ";
    } else {
	print RTF $Font{"PRE"}, " ";
    }
}

sub end_tt {
    local ($element) = @_;
    if($inheader > 0) {
	print RTF "\\f0\\b0", " ";
    } else {
	print RTF $Font{"P"}, " ";
    }
}


## List environments

$Begin{"UL"} = "begin_itemize";
$End{"UL"} = "end_list_env";

$Begin{"OL"} = "begin_enumerated";
$End{"OL"} = "end_list_env";

$Begin{"MENU"} = "begin_menu";
$End{"MENU"} = "end_list_env";

$Begin{"DIR"} = "begin_dir";
$End{"DIR"} = "end_list_env";

$Begin{"LI"} = "begin_list_item";
$End{"LI"} = "end_list_item";

sub html_begin_doc {
    @list_stack = ();
    $list_type = "UL";
    $list_counter = 0;
    $list_level = 0;
}

sub push_list_env {
    push (@list_stack, join (":", $list_type, $list_counter));
    $list_level++;
}

sub pop_list_env {
    ($list_type, $list_counter) = split (":", pop (@list_stack));
    $list_level--;
}

sub begin_itemize {
    local ($element, $tag) = @_;
    &push_list_env();
    $list_type="UL";
}

sub begin_menu {
    local ($element, $tag) = @_;
    &push_list_env();
    $list_type="MENU";
}

sub begin_dir {
    local ($element, $tag) = @_;
    &push_list_env();
    $list_type="DIR";
}

sub begin_enumerated {
    local ($element, $tag) = @_;
    &push_list_env();
    $list_type="OL";
}

sub end_list_env {
    local ($element) = @_;
    &pop_list_env();
    print RTF "\\sa240\\par\\pard\\plain\n";
}

sub begin_list_item {
    local ($element, $tag) = @_;
#    print RTF "\\sa240\\par\\pard\\plain\n";
    $indent = 360 * $list_level;
    $indent1 = $indent - 360;
#    print STDERR "List: $list_level\n";
    print RTF "\\par ";
    if($list_type eq "OL") {
	$list_counter++;
	print RTF $Font{"P"}," \n\\li$indent\\fi$indent1 $list_counter. ";
    } else {
	print RTF $Font{"P"}," \n\\li$indent ";
    }
}

sub end_list_item {
    local ($element) = @_;
}



$Begin{"DL"} = "begin_dl";
$End{"DL"} = "end_dl";
sub begin_dl {
    local ($element, $tag) = @_;
    $list_level++;
}
    
sub end_dl {
    local ($element) = @_;
    $list_level--;
    print RTF "\\sa240\\par\\pard\\plain\n";
}

$Begin{"DT"} = "begin_defined_term";
$End{"DT"} = "end_defined_term";
$Begin{"DD"} = "begin_defined_definition";
$End{"DD"} = "end_defined_definition";

sub begin_defined_term {
    local ($element, $tag) = @_;
    $indent = 360 * $list_level - 360;
    print RTF "\\par ", $Font{"P"}, "\\li$indent";
}

sub begin_defined_definition {
    local ($element, $tag) = @_;
    $indent = 360 * $list_level;
    print RTF "\\par ", $Font{"P"}, "\\li$indent";
}

sub end_defined_definition {
    local ($element) = @_;
}

sub end_defined_term {
    local ($element) = @_;
}

$Begin{"META"} = "begin_meta";

# a META tag sets a value in the assoc array %Variable
# i.e. <META name="author" content="Rushdie"> sers $Variable{author} to "Rushdie"
sub begin_meta {
    local ($element, $tag, *attributes) = @_;
    local ($variable, $value);
    $variable = $attributes{name};
    $value = $attributes{content};
    $Variable{$variable} = $value;}

$Begin{"IMG"} = "begin_img";

sub begin_img {
    local ($element, $tag, %attributes) = @_;
    if(exists $attributes{"src"}) {
	$image = $attributes{"src"};
	if($image =~ /(.*)\.([^.]+)/) {
	    $base = $1;
	    $suffix = $2;
	    print RTF "\\{bmc $base.bmp\\}";
	}
	$imagelist{$image}++;
    }
}

# Content and whitespace.

sub html_content {
    local ($string) = @_;
    if(defined $buildtitle) {
	$buildtitle .= " ";
	$buildtitle .= $string;
    }
    unless ($ignore_text) {
	$string =~ s/\{/\\{/o;
	$string =~ s/\}/\\}/o;
	if ($whitespace_significant) { # means verbatim
	    $string =~ s/\\"/\\\\"/o;
	}
	print RTF $string;
#	if(!(defined $in_a)) {print RTF " ";}
#	&print_word_wrap ($string);
    }
}

sub html_whitespace {
    local ($string) = @_;
    if ($whitespace_significant) { # means verbatim
	$string =~ s/\n/\\line\n/og;
    }
    print RTF $string;
}

# very minimal tables
$Begin{"TR"} = "begin_table_row";
$End{"TR"} = "end_table_row";
$Begin{"TH"} = "begin_table_head";
$End{"TH"} = "end_table_head";
$Begin{"TD"} = "begin_table_cell";
$End{"TD"} = "end_table_cell";
$Begin{"TABLE"} = "begin_table";
$End{"TABLE"} = "end_table";

sub begin_table {
    local ($element, $tag) = @_;
}

sub end_table {
    local ($element) = @_;
    print RTF "\\pard\n";
}

sub begin_table_row {
    local ($element, $tag) = @_;
    print RTF "\\trowd \\cellx2000 \\cellx10000\n";
    print RTF "\\pard \\intbl ", $Font{"P"}, "\n";
}

sub end_table_row {
    local ($element) = @_;
    print RTF "\\pard \\intbl \\row\n";
}

sub begin_table_head {
    local ($element, $tag) = @_;
}

sub end_table_head {
    local ($element) = @_;
    print RTF "\\cell\n";
}

sub begin_table_cell {
    local ($element, $tag) = @_;
}

sub end_table_cell {
    local ($element) = @_;
    print RTF "\\cell\n";
}


    
1;
