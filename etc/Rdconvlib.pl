# Subroutines for converting R documentation into text, HTML, LaTeX
# and R (Examples) format

# Copyright (C) 1997-2000 R Development Core Team
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the GNU
# General Public License for more details.
#
# A copy of the GNU General Public License is available via WWW at
# http://www.gnu.org/copyleft/gpl.html.	 You can also obtain it by
# writing to the Free Software Foundation, Inc., 59 Temple Place,
# Suite 330, Boston, MA  02111-1307  USA.

# Send any bug reports to R-bugs@lists.r-project.org


require "$R_HOME/etc/html-layout.pl";
use Text::Tabs;

# names of unique text blocks, these may NOT appear MORE THAN ONCE!
@blocknames = ("name", "title", "usage", "arguments", "format",
	       "description", "details", "value", "references", "source",
	       "seealso", "examples", "author", "note", "synopsis");

# These may appear multiply but are of simple structure:
@multiblocknames = ("alias", "keyword");


# These should NOT contain letters from $LATEX_SPEC
$NB = "normal-bracket";
$BN = "bracket-normal";
$EOB = "escaped-opening-bracket";
$ECB = "escaped-closing-bracket";
$ID = "$NB\\d+$BN";

$ECODE = "this-is-escaped-code";

$LATEX_SPEC = '\$\^&~_#';#-- these **should** be escaped in  text2latex(.)
$LATEX_SPECIAL = $LATEX_SPEC . '%\{\}\\\\';
$LATEX_DO_MATH = '-+\*/\|<>=!' . $LATEX_SPECIAL;
$MD = ',,,Math,del;;;'; #-- should NOT contain any characters from $LATEX_..
$Math_del = "\$"; #UNquoted '$'
$MAXLOOPS = 1000;


sub Rdconv { # Rdconv(foobar.Rd, type, debug, filename, pkgname)

    $Rdname = $_[0];
    open rdfile, "<$Rdname" || die "Rdconv(): Couldn't open '$Rdfile':$!\n";

    $type = $_[1];
    $debug = $_[2];
    $pkgname = $_[4];

    if($type !~ /,/) {
	## Trivial (R 0.62 case): Only 1 $type at a time ==> one filename is ok.
	## filename = 0	  <==>	STDOUT
	## filename = -1  <==>	do NOT open and close files!
	$htmlfile= $txtfile= $Sdfile= $latexfile= 
	    $Exfile = $chmfile = $_[3];
    } else { # have "," in $type: Multiple types with multiple output files
	$dirname = $_[3]; # The super-directory , such as  <Rlib>/library/<pkg>
	die "Rdconv(): '$dirname' is NOT a valid directory:$!\n"
	  unless -d $dirname;
	$htmlfile = $dirname ."/html/" .$Rdname.".html" if $type =~ /html/i;
	$txtfile= $dirname ."/help/" . $Rdname	        if $type =~ /txt/i;
	die "Rdconv(): type 'Sd' must not be used with other types (',')\n"
	  if $type =~ /Sd/i;
	$latexfile= $dirname ."/latex/". $Rdname.".tex"	if $type =~ /tex/i;
	$Exfile	  = $dirname ."/R-ex/" . $Rdname.".R"	if $type =~ /example/i;
    }
    

    $max_bracket = 0;
    $max_section = 0;

    undef $complete_text;
    undef %blocks;
    undef @section_body;
    undef @section_title;

    $skipping = 0;
    #-- remove comments (everything after a %)
    while(<rdfile>){
	$_ = expand $_;
	if (/^#ifdef\s+([A-Za-z0-9]+)/o) {
	    if ($1 ne $OSdir) { $skipping = 1; }
	    next;
	}
	if (/^#ifndef\s+([A-Za-z0-9]+)/o) {
	    if ($1 eq $OSdir) { $skipping = 1; }
	    next;
	}
	if (/^#endif/o) {
	    $skipping = 0;
	    next;
	}
	next if $skipping > 0;
	next if /^\s*%/o;#- completely drop full comment lines
	my $loopcount = 0;
	while(checkloop($loopcount++, $_, "\\%") &&
	      s/^\\%|([^\\])\\%/$1escaped_percent_sign/go){};
	s/^([^%]*)%.*$/$1/o;
	s/escaped_percent_sign/\\%/go;
	$complete_text .= $_;
    }
    printf STDERR "-- read file '%s';\n",$_[0] if $debug;

    mark_brackets();
    ##HARD Debug:print "$complete_text\n"; exit;
    escape_codes();
    if($debug) {
	print STDERR "\n--------------\nescape codes: '\@ecodes' =\n";

	while(my($id,$code) = each %ecodes) {
	    print STDERR "\t\$ec{$id}='$code'\n";
	}
    }

    if($type) {
	#-- These may be used in all cases :
	@aliases = get_multi($complete_text,"alias");
	@keywords= get_multi($complete_text,"keyword");

	get_blocks($complete_text);
 
	if($type =~ /html/i || $type =~ /txt/i ||
	   $type =~ /Sd/    || $type =~ /tex/i || $type =~ /chm/i ) {

	    get_sections($complete_text);

	} elsif($type =~ /example/i ) {
	    ;
	} else {
	    warn "\n** Rdconv --type '..' : no valid type specified\n";
	}

	rdoc2html($htmlfile)	if $type =~ /html/i;
	rdoc2txt($txtfile)	if $type =~ /txt/i;
	rdoc2Sd($Sdfile)	if $type =~ /Sd/i;
	rdoc2latex($latexfile)	if $type =~ /tex/i;
	rdoc2ex($Exfile)	if $type =~ /example/i;
	rdoc2chm($chmfile)	if $type =~ /chm/i;

    } else {
	warn "\n*** Rdconv(): no type specified\n";
    }
}


sub checkloop {
    my $loopcount = $_[0];
    my $text = $_[1];
    my $what = $_[2];

    if($loopcount > $MAXLOOPS){

	while($text =~ /$ECODE($ID)/){
	    my $id = $1;
	    my $ec = $ecodes{$id};
	    $text =~ s/$ECODE$id/$ec/;
	}
	$text = unmark_brackets($text);

	die("\n\n******* Syntax error: $what in\n/-----\n$text\\-----\n");
    }

    1;
}



# Mark each matching opening and closing bracket with a unique id.
# Idea and original code from latex2html
sub mark_brackets {

    $complete_text =~ s/^\\{|([^\\])\\{/$1$EOB/gso;
    $complete_text =~ s/^\\}|([^\\])\\}/$1$ECB/gso;

    print STDERR "\n-- mark_brackets:" if $debug;
    my $loopcount = 0;
    while(checkloop($loopcount++, $complete_text,
		    "mismatched or missing brackets") &&
	  $complete_text =~ /{([^{}]*)}/s){
	my $id = $NB . ++$max_bracket . $BN;
	$complete_text =~ s/{([^{}]*)}/$id$1$id/s;
	print STDERR "." if $debug;
    }
}


sub unmark_brackets {
    my $text = $_[0];

    my $loopcount = 0;
    while(($loopcount++ < $MAXLOOPS) && $text =~ /($ID)(.*)($ID)/s){
	$id = $1;
	if($text =~ s/$id(.*)$id/\{$1\}/s){
	    $text =~ s/$id(.*)$id/\{$1\}/so;
	}
	else{
	    # return $text;
	    $text =~ s/$id/\{/so;
	}
    }
    $text =~ s/$EOB/\{/gso;
    $text =~ s/$ECB/\}/gso;
    $text;
}

sub escape_codes {
    print STDERR "\n-- escape_codes:" if $debug;
    my $loopcount = 0;
    while(checkloop($loopcount++, $complete_text,
		    "while replacing all \\code{...}") &&
	  $complete_text =~ /\\code/){
	my ($id, $arg)	= get_arguments("code", $complete_text, 1);
	$complete_text =~ s/\\code$id(.*)$id/$ECODE$id/s;
	$ecodes{$id} = $1;
	print STDERR "," if $debug;
    }
}


# Write documentation blocks such as title, usage, etc. into the
# global hash array %blocks
sub get_blocks {

    my $text = $_[0];

    my $id="";
    print STDERR "--- Blocks\n" if $debug;
    foreach $block (@blocknames){
	if($text =~ /\\($block)($ID)/){
	    ($id, $blocks{$block}) = get_arguments($block, $text, 1);
	    print STDERR "found: $block\n" if $debug;
	    if((($block =~ /usage/) || ($block =~ /examples/))) {
		## multiple empty lines to one
		$blocks{$block} =~ s/^[ \t]+$//;
		$blocks{$block} =~ s/\n\n\n/\n\n/gom;
	    } else {
		## remove leading and trailing whitespace
		$blocks{$block} =~ s/^\s+//so;
		$blocks{$block} =~ s/\s+$//so;
		$blocks{$block} =~ s/\n[ \t]+/\n/go;
	    }

	    # no formatting commands allowed in the title string
	    if($block =~ /title/) {
		if($blocks{"title"} =~ /$ID/){
		    die("\nERROR: Environment ".
			"(text enclosed in \{\}) found in \\title\{...\}.\n".
			"The title must be plain text!\n\n");
		}
	    }

	}
    }
    print STDERR "---\n" if $debug;
}

# Get  ALL  multiblock things -- their simple arg. is put in array:
#
sub get_multi {

    my ($text, $name) = @_; # name: "alias" or "keyword"
    my @res, $k=0;
    print STDERR "--- Multi: $name\n" if $debug;
    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\name")
	  && $text =~ /\\$name($ID)/) {
	my $id = $1;
	my ($endid, $arg) =
	    get_arguments($name, $text, 1);
	print STDERR "found:" if $debug && $k==0;
	print STDERR " $k:$arg" if $debug;
	$arg =~ s/^\s*(\S)/$1/;
	$arg =~ s/\n[ \t]*(\S)/\n$1/g;
	$arg =~ s/\s*$//;
	$res[$k++] = $arg;
	$text =~ s/\\$name//s;
    }
    print STDERR "\n---\n" if $debug;
    @res;
}

# Write the user defined sections into the
# global hashs @section_body and @section_title
sub get_sections {

    my $text = $_[0];

    print STDERR "--- Sections\n" if $debug;
    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\section") &&
	  $text =~ /\\section($ID)/){
	my $id = $1;
	my ($endid, $section, $body)
	    = get_arguments("section", $text, 2);
	print STDERR "found: $section\n" if $debug;

	## remove leading and trailing whitespace
	$section =~ s/^\s+//so;
	$section =~ s/\s+$//so;
	$body =~ s/^\s+//so;
	$body =~ s/\s+$//so;
	$body =~ s/\n[ \t]+/\n/go;
	$section_body[$max_section] = $body;
	$section_title[$max_section++] = $section;
	$text =~ s/\\section//s;
    }
    print STDERR "---\n" if $debug;
}

# Get the arguments of a command.
sub get_arguments {

    my ($command, $text, $nargs) = @_;
    # Arguments of get_arguments:
    #  1, command: next occurence of `command' is searched
    #  2, text	 : `text' is the text containing the command
    #  3, nargs	 : the optional number of arguments to be extracted; default 1
    my @retval;
    # Returns a list with the id of the last closing bracket and
    # the arguments.

    if($text =~ /\\($command)(\[[^\]]+\])?($ID)/){
	$id = $3;
	$text =~ /$id(.*)$id/s;
	$retval[1] = $1;
	my $k=2;
	while(($k<=$nargs) && ($text =~ /$id($ID)/)){
	    $id = $1;
	    $text =~ /$id\s*(.*)$id/s;
	    $retval[$k++] = $1;
	}
    }
    $retval[0] = $id;
    @retval;
}

# Get the argument(s) of a link.
sub get_link {
    my ($text) = @_;
    my @retval, $id;
    if($text =~ /\\link\[([^\]]+)\]($ID)/){
	$retval[2] = $1;
	$id = $2;
	$text =~ /$id(.*)$id/s;
	$retval[1] = $1;
    } elsif($text =~ /\\link($ID)/){
	$id = $1;
	$text =~ /$id(.*)$id/s;
	$retval[1] = $1;
    }
    $retval[0] = $id;
    @retval;
}

# Print a short vector of strings  (utility).
sub print_vec {
    my($F, $nam, $do_nam, $sep, $end) = @_;
    my($i)=0;
    $sep = ', '	 unless $sep;
    $end = ".\n" unless $end;
    print $F "\@$nam = " if $do_nam;
    foreach (@$nam) { print $F ($i>0 ? $sep : '') . "'$_'"; $i++ }
    print $F $end;
}



# Print the hash %blocks ... for debugging only (I just insert this
# function manually at places where I need it :-)
sub print_blocks {

    while(($block,$text) = each %blocks) {

	print STDERR "\n\n********** $block **********\n\n";
	print STDERR $text;
    }
    print STDERR "\n";
}



# Drop the command and leave it's inside argument, i.e.,
#  replace "\abc{longtext}"
#  by		"longtext"
sub undefine_command {

    my ($text, $cmd) = @_;

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\$cmd") &&  $text =~ /\\$cmd/){
	my ($id, $arg)	= get_arguments($cmd, $text, 1);
	$text =~ s/\\$cmd(\[.*\])?$id(.*)$id/$2/s;
    }
    $text;
}


# Drop the command  AND	 it's inside argument, i.e.,
#  replace "_text1_\abc{longtext}-text2-" by "_text1_-text2-"
sub drop_full_command {

    my ($text, $cmd) = @_;
    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\$cmd") &&  $text =~ /\\$cmd/){
	my ($id, $arg)	= get_arguments($cmd, $text, 1);
	$text =~ s/\\$cmd$id.*$id//s;
    }
    $text;
}


# Replace the command and and its closing bracket
# by  $before  and  $after, respectively, e.g.,
#  replace "\abc{longtext}"
#  by	    "<Bef>longtext<Aft>"
sub replace_command {

    my ($text, $cmd, $before, $after) = @_;

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\$cmd") &&
	  $text =~ /\\$cmd/){
	my ($id, $arg)	= get_arguments($cmd, $text, 1);
	$text =~ s/\\$cmd$id(.*)$id/$before$1$after/s;
    }
    $text;
}

# Replace the command and and its closing bracket
# by  $before  and  $after, respectively, AND PREPEND a comment
# to eacho LINE e.g.,
#  replace "\abc{line1\nline2\n....}"
#  by	    "<Bef>\n##line1\n##line2\n##....<Aft>"
sub replace_prepend_command {

    my ($text, $cmd, $before, $after, $prepend) = @_;

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\$cmd") &&
	  $text =~ /\\$cmd/){
	my ($id, $arg)	= get_arguments($cmd, $text, 1);
	$text =~ /\\$cmd$id(.*)$id/s;
	$arg = $1;
	$arg =~ s/^/$prepend/gmo;# prepend at all line beginnings
	$arg =~ s/^$prepend//;   # but NOT the very beginning..
	$text =~ s/\\$cmd$id.*$id/$before$arg$after/s;
    }
    $text;
}

sub striptitle { # text
    my $text = $_[0];
    $text =~ s/\\//go;
    $text =~ s/---/-/go;
    $text =~ s/--/-/go;
    return $text;
}

#==************************ HTML ********************************


sub rdoc2html { # (filename) ; 0 for STDOUT

    if($_[0]!= -1) {
      if($_[0]) { open htmlout, "> $_[0]"; } else { open htmlout, "| cat"; }
    }
    $using_chm = 0;
    print htmlout html_functionhead(striptitle($blocks{"title"}), $pkgname,
				    $blocks{"name"});

    html_print_block("description", "Description");
    html_print_codeblock("usage", "Usage");
    html_print_argblock("arguments", "Arguments");
    html_print_block("format", "Format");
    html_print_block("details", "Details");
    html_print_argblock("value", "Value");

    html_print_sections();

    html_print_block("note", "Note");
    html_print_block("author", "Author(s)");
    html_print_block("source", "Source");
    html_print_block("references", "References");
    html_print_block("seealso", "See Also");
    html_print_codeblock("examples", "Examples");

    print htmlout html_functionfoot();
    close htmlout;
}


# Convert a Rdoc text string to HTML, i.e., convert \lang to <tt> etc.
sub text2html {

    my $text = $_[0];
    my $angles = $_[1]; # should < and > be converted?

    if($angles) {
        $text =~ s/&([^#])/&amp;\1/go; # might have explicit &# in source
	$text =~ s/>/&gt;/go;
	$text =~ s/</&lt;/go;
	$text =~ s/\\le/&lt;=/go;
	$text =~ s/\\ge/&gt;=/go;
	$text =~ s/\\%/%/go;

	$text =~ s/\n\s*\n/\n<P>\n/sgo;
	$text =~ s/\\dots/.../go;
	$text =~ s/\\ldots/.../go;

	$text =~ s/\\Gamma/&Gamma;/go;
	$text =~ s/\\alpha/&alpha;/go;
	$text =~ s/\\Alpha/&Alpha;/go;
	$text =~ s/\\pi/&pi;/go;
	$text =~ s/\\mu/&mu;/go;
	$text =~ s/\\sigma/&sigma;/go;
	$text =~ s/\\Sigma/&Sigma;/go;
	$text =~ s/\\lambda/&lambda;/go;
	$text =~ s/\\beta/&beta;/go;
	$text =~ s/\\epsilon/&epsilon;/go;
	$text =~ s/\\left\(/\(/go;
	$text =~ s/\\right\)/\)/go;
	$text =~ s/\\R/<FONT FACE=\"Courier New,Courier\" COLOR=\"\#666666\"><b>R<\/b><\/FONT>/go;
	$text =~ s/---/&#151;/go; # HTML 4.01 has &mdash; and &#8212;
	$text =~ s/--/&#150;/go; # HTML 4.01 has &ndash; and &#8211;
	$text =~ s/$EOB/\{/go;
	$text =~ s/$ECB/\}/go;
    }

    $text = replace_command($text, "emph", "<EM>", "</EM>");
    $text = replace_command($text, "bold", "<B>", "</B>");
    $text = replace_command($text, "file", "`<tt>", "</tt>'");

    $text = html_tables($text);
    $text =~ s/\\cr/<BR>/sgo;

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\link")
	  &&  $text =~ /\\link/){
	my ($id, $arg, $org) = get_link($text);
	## fix conversions in key of htmlindex:
	my $argkey = $arg;
	$argkey =~ s/&lt;/</go;
	$argkey =~ s/&gt;/>/go;
	$htmlfile = $htmlindex{$argkey};
	if($htmlfile){
	    if($using_chm) {
		if ($htmlfile =~ s+^$pkg/html/++) {
		    # in the same chm file
		} else {
		    $tmp = $htmlfile;
		    ($base, $topic) = ($tmp =~ m+(.*)/(.*)+);
		    $base =~ s+/html$++;
		    $htmlfile = "ms-its:../../$base/chtml/$base.chm::/$topic";
#		    print "$htmlfile\n";
		}
		$text =~
		    s/\\link(\[.*\])?$id.*$id/<A HREF=\"$htmlfile\">$arg<\/A>/s;
	    } else {
		$text =~
		    s/\\link(\[.*\])?$id.*$id/<A HREF=\"..\/..\/$htmlfile\">$arg<\/A>/s;
	    }
	}
	else {
	    $misslink = $misslink . " " . $argkey unless $opt ne "";
	    if($using_chm){
		if($opt ne "") {
		    my ($pkg, $topic) = split(/:/, $opt);
		    $topic = $arg if $topic eq "";
		    $opt =~ s/:.*$//o;
		    $htmlfile = "ms-its:../../$opt/chtml/$opt.chm::/$topic.html";
		    $text =~ s/\\link(\[.*\])?$id.*$id/<A HREF=\"$htmlfile\">$arg<\/A>/s;
		} else {
		    $text =~ s/\\link(\[.*\])?$id.*$id/$arg/s;
		} 
	    }
	    else{
		if($opt ne "") {
		    my ($pkg, $topic) = split(/:/, $opt);
		    $topic = $arg if $topic eq "";
		    $htmlfile = $pkg."/html/".$topic.".html";
		    $text =~ s/\\link(\[.*\])?$id.*$id/<A HREF=\"..\/..\/$htmlfile\">$arg<\/A>/s;
		} else {
		    $text =~ s/\\link(\[.*\])?$id.*$id/<A HREF=\"..\/..\/..\/doc\/html\/search\/SearchObject.html?$argkey\">$arg<\/A>/s;
		}
	    }
	}
    }

    $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\email")
	  &&  $text =~ /\\email/){
	my ($id, $arg)	= get_arguments("email", $text, 1);
	$text =~ s/\\email$id.*$id/<A HREF=\"mailto:$arg\">$arg<\/A>/s;
    }

    $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\url")
	  &&  $text =~ /\\url/){
	my ($id, $arg)	= get_arguments("url", $text, 1);
	$text =~ s/\\url.*$id/<A HREF=\"$arg\">$arg<\/A>/s;
    }

    # handle equations:
    $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\eqn")
	  &&  $text =~ /\\eqn/){
	my ($id, $eqn, $ascii) = get_arguments("eqn", $text, 2);
	$eqn = $ascii if $ascii;
	$text =~ s/\\eqn(.*)$id/<I>$eqn<\/I>/s;
    }

    $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\deqn")
	  &&  $text =~ /\\deqn/){
	my ($id, $eqn, $ascii) = get_arguments("deqn", $text, 2);
	$eqn = $ascii if $ascii;
	$text =~ s/\\deqn(.*)$id/<P align="center"><I>$eqn<\/I><\/P>/s;
    }

    $text = replace_command($text, "itemize", "<UL>", "</UL>");
    $text = replace_command($text, "enumerate", "<OL>", "</OL>");
    $text =~ s/\\item\s+/<li>/go;

    # handle "\describe"
    $text = replace_command($text, "describe", "<DL>", "</DL>");
    while(checkloop($loopcount++, $text, "\\item") && $text =~ /\\itemnormal/s)
    {
	my ($id, $arg, $desc)  = get_arguments("item", $text, 2);
	$descitem = "<DT>" . text2html($arg, 0) . "</DT>";
	$descitem .= "<DD>" . text2html($desc, 0) . "</DD>";
	$text =~ s/\\itemnormal.*$id/$descitem/s;
    }
    $text =~ s/\\([^\\])/$1/go;#-drop single "\" (as in ``\R'')
    $text =~ s/\\\\/\\/go;
    $text = html_unescape_codes($text);
    unmark_brackets($text);
}

sub code2html {

    my $text = $_[0];

    $text =~ s/&/&amp;/go;
    $text =~ s/>/&gt;/go;
    $text =~ s/</&lt;/go;
    $text =~ s/\\%/%/go;
    $text =~ s/\\ldots/.../go;
    $text =~ s/\\dots/.../go;

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\link")
	  &&  $text =~ /\\link/){
	my ($id, $arg, $opt) = get_link($text);

	## fix conversions in key of htmlindex:
	my $argkey = $arg;
	$argkey =~ s/&lt;/</go;
	$argkey =~ s/&gt;/>/go;
	$htmlfile = $htmlindex{$argkey};

	if($htmlfile){
	    if($using_chm) {
		if ($htmlfile =~ s+^$pkg/html/++) {
		    # in the same chm file
		} else {
		    $tmp = $htmlfile;
		    ($base, $topic) = ($tmp =~ m+(.*)/(.*)+);
		    $base =~ s+/html$++;
		    $htmlfile = "ms-its:../../$base/chtml/$base.chm::/$topic";
#		    print "$htmlfile\n";
		}
		$text =~
		    s/\\link(\[.*\])?$id.*$id/<A HREF=\"$htmlfile\">$arg<\/A>/s;
	    } else {
		$text =~
		    s/\\link(\[.*\])?$id.*$id/<A HREF=\"..\/..\/$htmlfile\">$arg<\/A>/s;
	    }
	}
	else{
	    $misslink = $misslink . " " . $argkey unless $opt ne "";
	    if($using_chm){
		if($opt ne "") {
		    my ($pkg, $topic) = split(/:/, $opt);
		    $topic = $arg if $topic eq "";
		    $opt =~ s/:.*$//o;
		    $htmlfile = "ms-its:../../$opt/chtml/$opt.chm::/$topic.html";
#		    print "$htmlfile\n";
       		    $text =~ s/\\link(\[.*\])?$id.*$id/<A HREF=\"$htmlfile\">$arg<\/A>/s;
		} else {
		    $text =~ s/\\link(\[.*\])?$id.*$id/$arg/s;
		} 
	    }
	    else{
		if($opt ne "") {
		    my ($pkg, $topic) = split(/:/, $opt);
		    $topic = $arg if $topic eq "";
		    $htmlfile = $pkg."/html/".$topic.".html";
		    $text =~ s/\\link(\[.*\])?$id.*$id/<A HREF=\"..\/..\/$htmlfile\">$arg<\/A>/s;
		} else {
		    $text =~ s/\\link(\[.*\])?$id.*$id/<A HREF=\"..\/..\/..\/doc\/html\/search\/SearchObject.html?$argkey\">$arg<\/A>/s;
		}
	    }
	}
    }

    $text = undefine_command($text, "dontrun");
    $text = drop_full_command($text, "testonly");
    $text =~ s/\\\\/\\/go;

    unmark_brackets($text);
}

# Print a standard block
sub html_print_block {

    my ($block,$title) = @_;

    if(defined $blocks{$block}){
	print htmlout html_title3($title);
	print htmlout "<p>\n", text2html($blocks{$block}, 1), "\n";
    }
}

# Print a code block (preformatted)
sub html_print_codeblock {

    my ($block,$title) = @_;

    if(defined $blocks{$block}){
	print htmlout html_title3($title);
	print htmlout "<PRE>";
	print htmlout code2html($blocks{$block});
	print htmlout "</PRE>\n\n";
    }
}


# Print the value or arguments block
sub html_print_argblock {

    my ($block,$title) = @_;

    if(defined $blocks{$block}){
	print htmlout html_title3($title);

	my $text = $blocks{$block};

	if($text =~ /\\item/s){
	    $text =~ /^(.*)(\\item.*)*/s;
	    my ($begin, $rest) = split(/\\item/, $text, 2);
	    if($begin){
		print htmlout text2html($begin, 1);
		$text =~ s/^$begin//s;
	    }
	    print htmlout "<TABLE>\n";
	    my $loopcount = 0;
	    while(checkloop($loopcount++, $text, "\\item")
		  && $text =~ /\\item/s){
		my ($id, $arg, $desc)  =
		    get_arguments("item", $text, 2);
		print htmlout "<TR VALIGN=\"TOP\"><TD><CODE>";
		print htmlout text2html($arg, 1);
		print htmlout "</CODE></TD>\n<TD>\n";
		print htmlout text2html($desc, 1), "</TD></TR>\n";
		$text =~ s/.*$id//s;
	    }
	    print htmlout "</TABLE>\n";
	    print htmlout "<P>\n", text2html($text, 1), "</P>\n";
	}
	else{
	    print htmlout "<P>\n", text2html($text, 1), "</P>\n";
	}
    }
}

# Print sections
sub html_print_sections {

    my $section;

    for($section=0; $section<$max_section; $section++){
	print htmlout html_title3($section_title[$section]);
	print htmlout "<P>\n", text2html($section_body[$section], 1), "</P>\n";
    }
}


sub html_unescape_codes {

    my $text = $_[0];

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "escaped code")
	  && $text =~ /$ECODE($ID)/){
	my $id = $1;
	my $ec = code2html($ecodes{$id});
	$text =~ s/$ECODE$id/<CODE>$ec<\/CODE>/;
    }
    $text;
}


sub html_tables {

    my $text = $_[0];

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\tabular")
	  &&  $text =~ /\\tabular/){

	my ($id, $format, $arg)	 =
	    get_arguments("tabular", $text, 2);

	$arg =~ s/\n/ /sgo;

	# remove trailing \cr (otherwise we get an empty last line)
	$arg =~ s/\\cr\s*$//go;

	# parse the format of the tabular environment
	my $ncols = length($format);
	my @colformat = ();
	for($k=0; $k<$ncols; $k++){
	    my $cf = substr($format, $k, 1);

	    if($cf =~ /l/o){
		$colformat[$k] = "left";
	    }
	    elsif($cf =~ /r/o){
		$colformat[$k] = "right";
	    }
	    elsif($cf =~ /c/o){
		$colformat[$k] = "center";
	    }
	    else{
		die("Error: unknown identifier \{$cf\} in" .
		    " tabular format \{$format\}\n");
	    }
	}

	# now do the real work: split into lines and columns
	my $table = "<TABLE>\n";
	my @rows = split(/\\cr/, $arg);
	for($k=0; $k<=$#rows;$k++){
	    $table .= "<TR>\n";
	    my @cols = split(/\\tab/, $rows[$k]);
	    die("Error:\n  $rows[$k]\\cr\n" .
		"does not fit tabular format \{$format\}\n")
		if ($#cols != $#colformat);
	    for($l=0; $l<=$#cols; $l++){
		$table .= "  <TD align=\"$colformat[$l]\">$cols[$l]</TD>";
	    }
	    $table .= "\n</TR>\n";
	}
	$table .= "</TABLE>\n";
	$text =~ s/\\tabular.*$id/$table/s;
    }

    $text;
}


#==************************** txt ******************************

use Text::Wrap;
use Text::Tabs;

sub rdoc2txt { # (filename); 0 for STDOUT

    if($_[0]!= -1) {
      if($_[0]) { open txtout, "> $_[0]"; } else { open txtout, "| cat"; }
    }

    $Text::Wrap::columns=72;
    $INDENT = 3;  # indent for \itemize and \enumerate first line
    $INDENTD = 0; # indent for \describe list first line
    $INDENTDD = 7; # indent for \describe list bodies

    if ($pkgname) {
	my $pad = 75 - length($blocks{"name"}) - length($pkgname) - 30;
	$pad = int($pad/2);
	print txtout  $blocks{"name"}, " " x $pad, 
	"package:$pkgname", " " x $pad,"R Documentation\n\n";
    }
    print txtout txt_header(striptitle($blocks{"title"})), "\n";
    txt_print_block("description", "Description");
    txt_print_codeblock("usage", "Usage");
    txt_print_argblock("arguments", "Arguments");
    txt_print_block("format", "Format");
    txt_print_block("details", "Details");
    txt_print_argblock("value", "Value");

    txt_print_sections();

    txt_print_block("note", "Note");
    txt_print_block("author", "Author(s)");
    txt_print_block("source", "Source");
    txt_print_block("references", "References");
    txt_print_block("seealso", "See Also");
    txt_print_codeblock("examples", "Examples");

    print txtout "\n";
    close txtout;
}

## Underline section headers
sub txt_header {

    my $header = $_[0];
    $header =~ s/\\//go;
#    '_' . join '_', split //, $header;
    my @letters = split //, $header;
    my $out = "", $a;
    for($l = 0; $l <= $#letters; $l++){
	$a = @letters[$l];
	if($a =~ /[A-Za-z0-9]/) {
	    $out .= '_' . $a;
	} else {
	    $out .= $a;
	}
    }
    return $out;
}

### Convert a Rdoc text string to txt
###   $_[0]: text to be converted
###   $_[1]: (optional) indentation of paragraphs. default = $INDENT

sub text2txt {

    my $text = $_[0];
    if($_[1]){
	my $indent = $_[1];
    }
    else{
	my $indent = $INDENT;
    }

    $text =~ s/^\.|([\n\(])\./$1\\\&./g;

    ## TABs are just whitespace
    $text =~ s/\t/ /g;

    $text = txt_tables($text);

    $text =~ s/\n\s*\n/\n\n/sgo;
    $text =~ s/\\dots/\\&.../go;
    $text =~ s/\\ldots/\\&.../go;
    $text =~ s/\\le/<=/go;
    $text =~ s/\\ge/>=/go;
    $text =~ s/\\%/%/sgo;
    $text =~ s/\\\$/\$/sgo;


    $text =~ s/\\Gamma/Gamma/go;
    $text =~ s/\\alpha/alpha/go;
    $text =~ s/\\Alpha/Alpha/go;
    $text =~ s/\\pi/pi/go;
    $text =~ s/\\mu/mu/go;
    $text =~ s/\\sigma/sigma/go;
    $text =~ s/\\Sigma/Sigma/go;
    $text =~ s/\\lambda/lambda/go;
    $text =~ s/\\beta/beta/go;
    $text =~ s/\\epsilon/epsilon/go;
    $text =~ s/\\left\(/\(/go;
    $text =~ s/\\right\)/\)/go;
    $text =~ s/\\R/R/go;
    $text =~ s/---/--/go;
    $text =~ s/--/-/go;
    $text =~ s/$EOB/\{/go;
    $text =~ s/$ECB/\}/go;

    $text = undefine_command($text, "link");
    $text = undefine_command($text, "emph");
    $text = undefine_command($text, "bold");
    $text = undefine_command($text, "textbf");
    $text = undefine_command($text, "mathbf");
    $text = undefine_command($text, "email");
    $text = replace_command($text, "file", "`", "'");
    $text = replace_command($text, "url", "<URL: ", ">");


    # handle equations:
    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\eqn")
	  &&  $text =~ /\\eqn/){
	my ($id, $eqn, $ascii) = get_arguments("eqn", $text, 2);
	$eqn = $ascii if $ascii;
	$eqn =~ s/\\([^&])/$1/go;
	$text =~ s/\\eqn(.*)$id/$eqn/s;
    }

    $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\deqn") &&  $text =~ /\\deqn/){
	my ($id, $eqn, $ascii) = get_arguments("deqn", $text, 2);
	$eqn = $ascii if $ascii;
	$eqn =~ s/\\([^&])/$1/go;
	$eqn =~ s/^\n*//o;
	$eqn =~ s/\n*$//o;
	$text =~ s/\\deqn(.*)$id/\n\n.DS B\n$eqn\n.DE\n\n/s;
    }

    $list_depth=0;

    $text = replace_command($text,
			    "itemize",
			    "\n.in +$INDENT\n",
			    "\n.in -$INDENT\n");

    $text = replace_command($text,
			    "enumerate",
			    "\n.inen +$INDENT\n",
			    "\n.inen -$INDENT\n");

    $text =~ s/\\item\s+/\n.ti * \n/go;

    # handle "\describe"
    $text = replace_command($text,
			    "describe",
			    "\n.in +$INDENTD\n",
			    "\n.in -$INDENTD\n");
    while(checkloop($loopcount++, $text, "\\item") && $text =~ /\\itemnormal/s)
    {
	my ($id, $arg, $desc)  = get_arguments("item", $text, 2);
	my $descitem = text2txt($arg);
	$descitem = "\n.tide " . $descitem . " \n". text2txt($desc);
	$text =~ s/\\itemnormal.*$id/$descitem/s;
    }

    $text = txt_unescape_codes($text);
    unmark_brackets($text);
}


sub code2txt {

    my $text = $_[0];

    $text =~ s/^\.|([\n\(])\./$1\\&./g;
    $text =~ s/\\%/%/go;
    $text =~ s/\\ldots/.../go;
    $text =~ s/\\dots/.../go;

    $text = undefine_command($text, "link");
    $text = undefine_command($text, "dontrun");
    $text = drop_full_command($text, "testonly");

    unmark_brackets($text);
}



# generate wrapped text, undo tabifying, zap empty lines to \n
sub mywrap {
    my ($pre1, $pre2, $text) = @_;

    my $ntext;
    if(length($text) > 0) {
	$ntext = wrap($pre1, $pre2, $text);
    } else {
	$ntext = $pre1;
    }
    my @lines = split /\n/, $ntext;
    my $out = "", $line;
    foreach $line (@lines) {
	$line = expand $line;
	$line =~ s/^\s+$//o;
	$out .= $line . "\n";
    }
    $out =~ s/\n$//;
    return $out;
}

# Print text indent and filled: will put out a leading blank line.
sub txt_fill { # pre1, base, "text to be formatted"
    my ($pre1, $base, $text) = @_;
    my $INDENT = $base;
    my $indent = " " x $INDENT;

# first split by paragraphs

    $text =~ s/\\\\/\\bsl{}/go;
    $text =~ s/\\&\./\./go; # unescape code pieces
# A mess:  map  & \& \\& \\\& to  & & \& \&
    $text =~ s/\\&/&/go;
    $text =~ s/\\ / /go;
    $text =~ s/\\_/_/go;
    $text =~ s/\\$/\$/go;
    $text =~ s/\\#/#/go;
    $text =~ s/\\%/%/go;
    $text =~ s/\\bsl{}/\\/go;

    my @paras = split /\n\n/, $text;
    $indent1 = $pre1; $indent2 = $indent;

    my $enumlevel = 0, @enum;
    foreach $para (@paras) {
	# strip leading white space
	$para  =~ s/^\s+//;
	my $para0 = $para;
	$para0 =~ s/\n\s*/ /go;
	# check for a item in itemize etc
	if ($para =~ s/^[\n]*\.ti //) {
	    $indent1 = $indent;
	    $indent2 = $indent1 . (" " x 3);
	    if ($enum{$enumlevel} > 0) {
		$para =~ s/\*/$enum{$enumlevel}./;
		$enum{$enumlevel} += 1;
	    }
	}
	# check for a item in describe etc
	if ($para =~ s/^[\n]*\.tide ([^\n]+)\n//) {
	    my $short = length($indent) + $INDENTDD - length($1);
	    if ($short >= 0) {
		$indent1 = " " x $short . $1;
	    } else {
		$indent1 = "  " . $1;
		$para = "\\cr ". $para;
	    }    
	    $indent2 = $indent . (" " x $INDENTDD);    
	}
        # check for .in or .inen command
	if ($para =~ s/^[\n]*\.in([^\ ]*) (.*)/\2/) {
	    $INDENT = $INDENT + $para;
	    $indent1 = $indent2 = $indent = " " x $INDENT;
	    if ($para > 0) {
		$enumlevel += 1;
		if ($1 =~ /en/) {
		    $enum{$enumlevel} = 1;
		} else {
		    $enum{$enumlevel} = 0;
		}
	    } else {
		$enumlevel -= 1;
	    }
        # check for a \deqn block
	} elsif ($para0 =~ s/^\s*\.DS B\s*(.*)\.DE/\1/) {
	    $para0 =~ s/\s*$//o;
	    if(length($para0) > 65) {
		print txtout "\n", " ", $para0, "\n";
	    } else {
		my $shift = int((70 - length($para0))/2);
		print txtout "\n", " " x $shift, $para0, "\n";
	    }

	# check for a \tabular block
	} elsif ($para =~ s/^\.TS\n//) {
	    my $format = $para;
	    $format =~ s/([rlc]*)\n.*/$1/o;
	    # parse the format of the tabular environment
	    my $ncols = length($format);
	    my @colformat = ();
	    for($k=0; $k<$ncols; $k++){
		my $cf = substr($format, $k, 1);
		
		if($cf =~ /l/o){
		    $colformat[$k] = "l";
		}
		elsif($cf =~ /r/o){
		    $colformat[$k] = "r";
		}
		elsif($cf =~ /c/o){
		    $colformat[$k] = "c";
		}
	    }
	    my @colwidths, $colwidth, $left, $right;
	    for($l = 0; $l < $#colformat; $l++){ $colwidths[$l] = 0; }

	    # now do the real work: split into lines and columns
	    # first scan them and get the field widths.
	    $para =~ s/([^\n]*)\n//o;
	    my @rows = split(/\\cr/, $para);
	    my $tmp, $line = "";
	    for($k = 0; $k <= $#rows; $k++){
		my @cols = split(/\\tab/, $rows[$k]);
		die("Error:\n  $rows[$k]\\cr\n" .
		    "does not fit tabular format \{$format\}\n")
		    if ($#cols != $#colformat);
		for($l = 0; $l <= $#cols; $l++){
		    $tmp = $cols[$l];
		    $tmp =~ s/^\s*//;
		    $tmp =~ s/\s*$//;
		    $colwidth = length($tmp);
		    if ($colwidth > $colwidths[$l]) { 
			$colwidths[$l] = $colwidth;
		    }
		}
	    }
	    print txtout "\n";
	    for($k = 0; $k <= $#rows; $k++){
		$line = "  "; # indent by two
		my @cols = split(/\\tab/, $rows[$k]);
		for($l = 0; $l <= $#cols; $l++){
		    $tmp = $cols[$l];
		    $tmp =~ s/^\s*//;
		    $tmp =~ s/\s*$//;
		    $colwidth = length($tmp);
		    if ($colformat[$l] eq "r") {
			$left = $colwidths[$l] - $colwidth;
		    } elsif ($colformat[$l] eq "c") {
			$left = int (($colwidths[$l] - $colwidth)/2);
		    } else {
			$left = 0;
		    }
		    # 2 is the column gap
		    $right = $colwidths[$l] - $colwidth + 2 - $left;
		    if ($l == $#cols) {
			$right = 0; # don't need to right-pad end.
		    }
		    $line .= " " x $left . $tmp . " " x $right;
		}
		print txtout $indent, "$line\n";
	    }

	# plain text    
	} else {
	    $para =~ s/\n\s*/ /go;
	    print txtout "\n";
	    # Now split by \cr blocks
	    my @blocks = split /\\cr/, $para;
	    foreach $text (@blocks) {
		$text =~ s/^\s+//o;
		print txtout mywrap($indent1, $indent2, $text), "\n";
		$indent1 = $indent2;
	    }
	}
    }
}

# Print a standard block
sub txt_print_block {

    my ($block,$title) = @_;
    my $next;

    if(defined $blocks{$block}){
	print txtout "\n";
	print txtout txt_header($title), ":\n";
	$ntext = text2txt($blocks{$block});
	txt_fill("     ", 5, $ntext);
    }
}

# Print a code block (preformatted)
sub txt_print_codeblock {

    my ($block,$title) = @_;
    my $ntext;
    my $indent = " " x 5;

    if(defined $blocks{$block}){
	print txtout "\n";
	print txtout txt_header($title), ":\n" if $title;
	$ntext = code2txt($blocks{$block});
	# make sure there is precisely one leading "\n"
	$ntext =~ s/^[\n]*//go;
	$ntext = "\n". $ntext;
	$ntext =~ s/\\&\././go;
	foreach $line (split /\n/, $ntext) {
	    $line =~ s/\\\\/\\/go;
	    $line =~ s/^\t/        /o;
	    $line =~ s/^\s+$//o;
	    if(length($line) > 0) {
		print txtout $indent, $line, "\n";
	    } else {
		print txtout "\n";
	    }
	}
    }
}


# Print the value or arguments block
sub txt_print_argblock {

    my ($block,$title) = @_;

    if(defined $blocks{$block}){

	print txtout "\n";
	print txtout txt_header($title), ":\n" if $title;

	my $text = $blocks{$block};

	if($text =~ /\\item/s){
	    $text =~ /^(.*)(\\item.*)*/s;
	    my ($begin, $rest) = split(/\\item/, $text, 2);
	    if($begin){
		txt_fill("     ", 5, text2txt($begin));
		$text =~ s/^$begin//s;
	    }
	    my $loopcount = 0;
	    while(checkloop($loopcount++, $text, "\\item") &&
		  $text =~ /\\item/s){
		my ($id, $arg, $desc)  = get_arguments("item", $text, 2);
		$arg = text2txt($arg);
		$arg =~ s/\\&//go;
		$desc = text2txt($desc);
		$arg0 = $arg.": ";
		$short = 10 - length($arg0);
		$arg0 = " " x $short. $arg0 if $short > 0;
		if (length($desc) > 0) {
		    txt_fill($arg0, 10, $desc);
		} else {
		    print txtout "\n", $arg0, "\n";
		}
		$text =~ s/.*$id//s;
	    }
	    txt_fill("     ", 5, text2txt($text));
	}
	else{
	    txt_fill("     ", 5, text2txt($text));
	}
    }
}

# Print sections
sub txt_print_sections {

    my $section;

    for($section=0; $section<$max_section; $section++){
	print txtout "\n";
	print txtout txt_header($section_title[$section]), ":\n";
	txt_fill("     ", 5, text2txt($section_body[$section]));
    }
}


sub txt_unescape_codes {

    my $text = $_[0];

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "escaped code")
	  && $text =~ /$ECODE($ID)/){
	my $id = $1;
	my $ec = code2txt($ecodes{$id});
	$text =~ s/$ECODE$id/\`$ec\'/;
    }
    $text;
}


sub txt_tables {

    my $text = $_[0];

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\tabular")
	  &&  $text =~ /\\tabular/){

	my ($id, $format, $arg)	 =
	    get_arguments("tabular", $text, 2);

	$arg =~ s/\n/ /sgo;

	# remove trailing \cr (otherwise we get an empty last line)
	$arg =~ s/\\cr\s*$//go;

	# parse the format of the tabular environment
	my $ncols = length($format);
	my @colformat = ();
	for($k=0; $k<$ncols; $k++){
	    my $cf = substr($format, $k, 1);

	    if($cf =~ /l/o){
		$colformat[$k] = "l";
	    }
	    elsif($cf =~ /r/o){
		$colformat[$k] = "r";
	    }
	    elsif($cf =~ /c/o){
		$colformat[$k] = "c";
	    }
	    else{
		die("Error: unknown identifier \{$cf\} in" .
		    " tabular format \{$format\}\n");
	    }
	}

	my $table = "\n\n.TS\n$format\n$arg\n\n";

	$text =~ s/\\tabular.*$id/$table/s;
    }

    $text;
}


#==**************************** Sd ******************************


sub rdoc2Sd { # (filename)

    if($_[0]!= -1) {
      if($_[0]) { open Sdout, "> $_[0]"; } else { open Sdout, "| cat"; }
    }
    print Sdout "\.\\\" -*- nroff -*- generated from \.Rd format\n";
    print Sdout ".BG\n";
    print Sdout ".FN ", $blocks{"name"}, "\n";
    print Sdout ".TL\n";
    print Sdout $blocks{"title"}, "\n";
    if (defined $blocks{"description"}){
	print Sdout ".DN\n";
	print Sdout text2nroff($blocks{"description"}), "\n";
    }
    if (defined $blocks{"usage"}){
	print Sdout ".CS\n";
	print Sdout text2nroff($blocks{"usage"}), "\n";
    }
    Sd_print_argblock("arguments", ".RA");
    Sd_print_argblock("value", ".RT");
    Sd_print_sections();
    Sd_print_block("note", "Note");
    Sd_print_block("references", ".SH REFERENCES");
    print Sdout "\n";
    Sd_print_block("seealso", ".SA");
    print Sdout "\n";
    Sd_print_codeblock("examples", ".EX");
    while ($#keywords >= 0) {
	print Sdout ".KW ", shift( @keywords ), "\n";
    }
    print Sdout ".WR\n";
    close Sdout;
}

# Convert a Rdoc text string to nroff
#   $_[0]: text to be converted
#   $_[1]: (optional) indentation of paragraphs. default = $INDENT

# Print a standard block

sub Sd_print_block {

    my ($block,$macro) = @_;

    if(defined $blocks{$block}){
	print Sdout $macro, "\n";
	print Sdout text2nroff($blocks{$block});
    }
}

# Print a code block (preformatted)
sub Sd_print_codeblock {

    my ($block, $macro) = @_;

    if(defined $blocks{$block}){
	print Sdout $macro;
	print Sdout code2nroff($blocks{$block});
    }
}


# Print the value or arguments block
sub Sd_print_argblock {

    my ($block, $macro) = @_;

    if(defined $blocks{$block}){
	print Sdout $macro, "\n" if $macro;
	my $text = $blocks{$block};

	if($text =~ /\\item/s){
	    $text =~ /^(.*)(\\item.*)*/s;
	    my ($begin, $rest) = split(/\\item/, $text, 2);
	    if($begin){
		print Sdout text2nroff($begin);
		$text =~ s/^$begin//s;
	    }
	    my $loopcount = 0;
	    while(checkloop($loopcount++, $text, "\\item") &&
		  $text =~ /\\item/s){
		my ($id, $arg, $desc)  = get_arguments("item", $text, 2);
		$arg = text2nroff($arg);
		$desc = text2nroff($desc);
		print Sdout ".AG ", $arg, "\n";
		print Sdout $desc, "\n";
		$text =~ s/.*$id//s;
	    }
	}
	else{
	    print Sdout text2nroff($text), "\n";
	}
    }
}

# Print sections
sub Sd_print_sections {

    my $section;

    for($section=0; $section<$max_section; $section++){
	print Sdout "\n";
	print Sdout ".SH ";
	print Sdout $section_title[$section], "\n";
	print Sdout text2nroff($section_body[$section]), "\n";
    }
}


#==********************* Example ***********************************


sub rdoc2ex { # (filename)

    local($tit = striptitle($blocks{"title"}));

    if(defined $blocks{"examples"}) {
	if($_[0]!= -1) {
	    if($_[0]) { open Exout, "> $_[0]"; } else { open Exout, "| cat"; }
	}
	$tit =~ s/\s+/ /g;
	print Exout "###--- >>> `"; print Exout $blocks{"name"};
	print Exout "' <<<----- "; print Exout $tit;
	print Exout "\n\n";
	if(@aliases) {
	    foreach (@aliases) {
		print Exout "\t## alias\t help($_)\n";
	    }
	    print Exout "\n";
	}

	ex_print_exampleblock("examples", "Examples");

	if(@keywords) {
	    print Exout "## Keywords: ";
	    &print_vec(Exout, 'keywords');
	}
	print Exout "\n\n";

	close Exout;
    }
}

sub ex_print_exampleblock {

    my ($block,$env) = @_;

    if(defined $blocks{$block}){
	print Exout "##___ Examples ___:\n";
	print Exout  code2examp($blocks{$block});
	print Exout "\n";
    }
}

sub code2examp {
    #-	similar to ..2latex
    my $text = $_[0];

    $text =~ s/\\%/%/go;
    $text =~ s/\\ldots/.../go;
    $text =~ s/\\dots/.../go;

    $text = undefine_command($text, "link");
    $text = undefine_command($text, "testonly");
    $text = replace_prepend_command($text, "dontrun","##Don't run: ", "",
				    "##D ");
    $text =~ s/\\\\/\\/g;

    unmark_brackets($text);
}


#==********************* LaTeX ***********************************

sub ltxstriptitle { # text
    my $text = $_[0];
    $text =~ s/\\R/\\R\{\}/go;
    return $text;
}
sub foldorder {uc($a) cmp uc($b) or $a cmp $b;}

sub rdoc2latex {# (filename)

    my $c, $a;

    if($_[0]!= -1) {
      if($_[0]) { open latexout, "> $_[0]"; } else { open latexout, "| cat"; }
    }
    print latexout "\\Header\{";
    print latexout $blocks{"name"};
    print latexout "\}\{";
    print latexout ltxstriptitle($blocks{"title"});
    print latexout "\}\n";

    my $current = $blocks{"name"}, $generic, $cmd;
    foreach (sort foldorder @aliases) {
	$generic = $a = $_;
	$generic =~ s/\.data\.frame$/.dataframe/o;
	$generic =~ s/\.model\.matrix$/.modelmatrix/o;
	$generic =~ s/\.[^.]+$//o;
	if ($generic ne "" && $generic eq $current && $generic ne "ar") { 
	    $cmd = "methalias"
	} else { $cmd = "alias"; $current = $a; }
	
	$c = code2latex($_,0);
	$a = latex_code_alias($c);
	print STDERR "rdoc2l: alias='$_', code2l(.)='$c', latex_c_a(.)='$a'\n"
	    if $debug;
	printf latexout "\\%s\{%s\}\{%s\}\n", $cmd, $a, $blocks{"name"}
	unless /^\Q$blocks{"name"}\E$/; # Q..E : Quote (escape) Metacharacters
    }
    foreach (@keywords) {
	printf latexout "\\keyword\{%s\}\{%s\}\n", $_, $blocks{"name"}
	unless /^$/ ;
    }
    latex_print_block("description", "Description");
    latex_print_codeblock("usage", "Usage");
    latex_print_argblock("arguments", "Arguments");
    latex_print_block("format", "Format");
    latex_print_block("details", "Details");
    latex_print_argblock("value", "Value");

    latex_print_sections();

    latex_print_block("note", "Note");
    latex_print_block("author", "Author");
    latex_print_block("source", "Source");
    latex_print_block("references", "References");
    latex_print_block("seealso", "SeeAlso");
    latex_print_exampleblock("examples", "Examples");

    print latexout "\n";
    close latexout;
}

# The basic translator for `normal text'
sub text2latex {

    my $text = $_[0];

    $text =~ s/$EOB/\\\{/go;
    $text =~ s/$ECB/\\\}/go;

    $text =~ s/\\itemize/\\Itemize/go;
    $text =~ s/\\enumerate/\\Enumerate/go;
    $text =~ s/\\tabular/\\Tabular/go;
    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\eqn")
	  &&  $text =~ /\\eqn/){
	my ($id, $eqn, $ascii) = get_arguments("eqn", $text, 2);
	# $ascii may be empty
	$text =~ s/\\eqn.*$id/\\eeeeqn\{$eqn\}\{$ascii\}/s;
    }

    $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\deqn")
	  &&  $text =~ /\\deqn/){
	my ($id, $eqn, $ascii) = get_arguments("deqn", $text, 2);
	$text =~ s/\\deqn.*$id/\\dddeqn\{$eqn\}\{$ascii\}/s;
    }

    $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\item") && $text =~ /\\itemnormal/s)
    {
	my ($id, $arg, $desc)  = get_arguments("item", $text, 2);
	$descitem = "\\DITEM[" . text2latex($arg) . "] " . text2latex($desc);
	$text =~ s/\\itemnormal.*$id/$descitem/s;
    }


    $text =~ s/\\eeeeqn/\\eqn/go;
    $text =~ s/\\dddeqn/\\deqn/og;
    $text =~ s/\\DITEM/\\item/og;

    $text =~ s/\\\\/\\bsl{}/go;
# A mess:  map  & \& \\& \\\& to  \& \& \bsl{}\& \bsl{}\&
    $text =~ s/([^\\])&/$1\\&/go;
    $text =~ s/\\R(\s+)/\\R\{\}$1/go;
    $text =~ s/\\cr\n\[/\\\\\{\}\n\[/go;
    $text =~ s/\\cr/\\\\/go;
    $text =~ s/\\tab(\s+)/&$1/go;

    ##-- We should escape $LATEX_SPEC  unless within `eqn' above ...
    ##-- this would escape them EVERYWHERE:
    ## $text =~ s/[$LATEX_SPEC]/\\$&/go;  #- escape them (not the "bsl" \)
    $text = latex_unescape_codes($text);
    unmark_brackets($text);
}

sub code2latex {

    my ($text, $hyper) = @_;

    $text =~ s/\\%/%/go;
    $text =~ s/\\ldots/.../go;
    $text =~ s/\\dots/.../go;

#    $text =~ s/\\\\/\\bsl{}/go;
    if($hyper) {
	my $loopcount = 0;
	while(checkloop($loopcount++, $text, "\\link")
	      && $text =~ /\\link/) {
	    my ($id, $arg, $opt) = get_link($text);
	    $text =~ s/\\link(\[.*\])?$id.*$id/HYPERLINK($arg)/s;
	}
    } else {
	$text = undefine_command($text, "link");
    }
    $text = undefine_command($text, "dontrun");
    $text = drop_full_command($text, "testonly");
    unmark_brackets($text);
}

sub latex_print_block {

    my ($block,$env) = @_;

    if(defined $blocks{$block}){
	print latexout "\\begin\{$env\}\\relax\n";
	print latexout text2latex($blocks{$block});
	print latexout "\\end\{$env\}\n";
    }
}

sub latex_print_codeblock {

    my ($block,$env) = @_;

    if(defined $blocks{$block}){
	print latexout "\\begin\{$env\}\n";
	print latexout "\\begin\{verbatim\}";
	print latexout code2latex($blocks{$block},0);
	print latexout "\\end\{verbatim\}\n";
	print latexout "\\end\{$env\}\n";
    }
}

sub latex_print_exampleblock {

    my ($block,$env) = @_;

    if(defined $blocks{$block}){
	print latexout "\\begin\{$env\}\n";
	print latexout "\\begin\{ExampleCode\}";
	my $out = code2latex($blocks{$block},0);
	$out =~ s/\\\\/\\/go;
	print latexout $out;
	print latexout "\\end\{ExampleCode\}\n";
	print latexout "\\end\{$env\}\n";
    }
}

sub latex_print_argblock {

    my ($block,$env) = @_;

    if(defined $blocks{$block}){

	print latexout "\\begin\{$env\}\n";

	my $text = $blocks{$block};

	if($text =~ /\\item/s){#-- if there is >= 1 "\item":  ldescription
	    $text =~ /^(.*)(\\item.*)*/s;
	    my ($begin, $rest) = split(/\\item/, $text, 2);
	    if($begin){
		print latexout text2latex($begin);
		$text =~ s/^$begin//s;
	    }
	    print latexout "\\begin\{ldescription\}\n";
	    my $loopcount = 0;
	    while(checkloop($loopcount++, $text, "\\item")
		  &&  $text =~ /\\item/s){
		my ($id, $arg, $desc)  = get_arguments("item", $text, 2);
		print latexout "\\item\[";
		print latexout latex_code_cmd(code2latex($arg,1));
		print latexout "\] ";
		print latexout text2latex($desc), "\n";
		$text =~ s/.*$id//s;
	    }
	    print latexout "\\end\{ldescription\}\n";
	    print latexout text2latex($text);
	}
	else{
	    print latexout text2latex($text);
	}
	print latexout "\\end\{$env\}\n";
    }
}

sub latex_print_sections {

    my $section;

    for($section=0; $section<$max_section; $section++){
	print latexout "\\begin\{Section\}\{" . $section_title[$section] . "\}\n";
	print latexout text2latex($section_body[$section]);
	print latexout "\\end\{Section\}\n";
    }
}

sub latex_unescape_codes {

    my $text = $_[0];

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "escaped code")
	  && $text =~ /$ECODE($ID)/){
	my $id = $1;
	my $ec = latex_code_cmd(code2latex($ecodes{$id},1));
	$text =~ s/$ECODE$id/$ec/;
    }
    $text;
}


# The next two should transform links and aliases identically
# so use common subroutines

sub latex_code_trans {
    my $c = $_[0];
    my $BSL = '@BSL@';

    if($c =~ /[$LATEX_SPECIAL]/){
	$c =~ s/\\\\/$BSL/go;
	$c =~ s/\\([$LATEX_SPECIAL])/$1/go; #- unescape them (should not be escaped)
	$c =~ s/[$LATEX_SPECIAL]/\\$&/go; #- escape them
	$c =~ s/\\\^/\$\\,\\hat{\\,}\$/go;# ^ is SPECIAL
	$c =~ s/\\~/\$\\,\\tilde{\\,}\$/go;
	$c =~ s/$BSL/\\bsl{}/go;
    }
    ## avoid conversion to guillemots
    $c =~ s/<</<\{\}</;
    $c =~ s/>>/>\{\}>/;
    $c =~ /HYPERLINK\(([^)]*)\)/;
    my $link = latex_link_trans($1);
    $c =~ s/HYPERLINK\([^)]*\)/\\Link{$link}/go;
    $c;
}

sub latex_link_trans {
    my $c = $_[0];
    $c =~ s/<-\./<\\Rdash\./go;
    $c =~ s/<-$/<\\Rdash/go;
    $c
}

sub latex_code_cmd {

    my $code = $_[0];

    warn("\nERROR: found `\@' in \\code{...\}\n") if $code =~ /@/;
    $code = latex_code_trans ($code);
    $code = "\\code\{" . $code . "\}";
    $code;
}


# Tough examples are
#	Logic.Rd  Arithmetic.Rd	 Extract.Rd  formula.Rd
sub latex_code_alias {

    my $c = $_[0];  ##-- $c is (typically) the OUTPUT of  code2latex(.) :
    $c = latex_code_trans ($c);
    $c = latex_link_trans ($c);
    $c =~ s/\!/"!/go; # "  This is the bibtex escape
    $c =~ s/\|/"|/go; # "
#      $c =~ s/@/"@/go; # "  Not currently valid R character
    $c;
}

#==************************ Compiled HTML ********************************


sub rdoc2chm { # (filename) ; 0 for STDOUT

    if($_[0]!= -1) {
      if($_[0]) { open htmlout, "> $_[0]"; } else { open htmlout, "| cat"; }
    }
    $using_chm = 1;
    print htmlout chm_functionhead(striptitle($blocks{"title"}), $pkgname,
				   $blocks{"name"});

    html_print_block("description", "Description");
    html_print_codeblock("usage", "Usage");
    html_print_argblock("arguments", "Arguments");
    html_print_block("format", "Format");
    html_print_block("details", "Details");
    html_print_argblock("value", "Value");

    html_print_sections();

    html_print_block("note", "Note");
    html_print_block("author", "Author(s)");
    html_print_block("source", "Source");
    html_print_block("references", "References");
    html_print_block("seealso", "See Also");
    html_print_codeblock("examples", "Examples");

    print htmlout html_functionfoot();
    close htmlout;
    $using_chm = 0;
}

# Local variables: **
# perl-indent-level: 4 **
# cperl-indent-level: 4 **
# page-delimiter: "^#==" **
# End: **
