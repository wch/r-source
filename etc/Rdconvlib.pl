# Subroutines for converting R documentation into HTML, LaTeX, Nroff
# and R (Examples) format

# Copyright (C) 1997 Friedrich Leisch
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

# Send any bug reports to Friedrich.Leisch@ci.tuwien.ac.at

# Bugs: still get ``\bsl{}'' in verbatim-like, see e.g. Examples of apropos.Rd

## New: \verbatim{}: like \examples{}, but can appear 0-n times [MM].
## ---	===========
## Original idea:  Can have *SEVERAL* verbatim	 codeblocks which should
## appear  (almost) WHERE they were initially !!
## BUT, this is not really possible:
##	we collect the block into a hash array and don't even remember
##	their order in the *.Rd file
##
## ==> Consequence: Allow \verbatim{ ...}  only *within* other
##     top-level blocks ...

require "$R_HOME/etc/html-layout.pl";


# names of unique text blocks, these may NOT appear MORE THAN ONCE!
@blocknames = ("name", "title", "usage", "arguments", "format",
	       "description", "details", "value", "references", "source",
	       "seealso", "examples", "author", "note");

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
	$htmlfile= $nrofffile= $Sdfile= $latexfile= $Exfile = $_[3];
    } else { # have "," in $type: Multiple types with multiple output files
	$dirname = $_[3]; # The super-directory , such as  <Rlib>/library/<pkg>
	die "Rdconv(): '$dirname' is NOT a valid directory:$!\n"
	  unless -d $dirname;
	$htmlfile = $dirname ."/html/" .$Rdname.".html" if $type =~ /html/i;
	$nrofffile= $dirname ."/help/" . $Rdname	if $type =~ /nroff/i;
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

	if($type =~ /html/i || $type =~ /nroff/i ||
	   $type =~ /Sd/    || $type =~ /tex/i) {

	    get_sections($complete_text);

	} elsif($type =~ /example/i ) {
	    ;
	} else {
	    warn "\n** Rdconv --type '..' : no valid type specified\n";
	}

	rdoc2html($htmlfile)	if $type =~ /html/i;
	rdoc2nroff($nrofffile)	if $type =~ /nroff/i;
	rdoc2Sd($Sdfile)	if $type =~ /Sd/i;
	rdoc2latex($latexfile)	if $type =~ /tex/i;
	rdoc2ex($Exfile)	if $type =~ /example/i;

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

    if($text =~ /\\($command)($ID)/){
	$id = $2;
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
	$text =~ s/\\$cmd$id(.*)$id/$1/s;
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
	$text =~ s/\\$cmd$id.*$id/$`$'/s;
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


#==************************ HTML ********************************


sub rdoc2html { # (filename) ; 0 for STDOUT

    if($_[0]!= -1) {
      if($_[0]) { open htmlout, "> $_[0]"; } else { open htmlout, "| cat"; }
    }
    print htmlout html_functionhead($blocks{"title"}, $pkgname,
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

    $text =~ s/&/&amp;/go;
    $text =~ s/>/&gt;/go;
    $text =~ s/</&lt;/go;
    $text =~ s/\\le/&lt;=/go;
    $text =~ s/\\ge/&gt;=/go;
    $text =~ s/\\%/%/go;

    $text =~ s/\n\s*\n/\n<P>\n/sgo;
    $text =~ s/\\dots/.../go;
    $text =~ s/\\ldots/.../go;

    $text =~ s/\\Gamma/&Gamma/go;
    $text =~ s/\\alpha/&alpha/go;
    $text =~ s/\\Alpha/&Alpha/go;
    $text =~ s/\\pi/&pi/go;
    $text =~ s/\\mu/&mu/go;
    $text =~ s/\\sigma/&sigma/go;
    $text =~ s/\\Sigma/&Sigma/go;
    $text =~ s/\\lambda/&lambda/go;
    $text =~ s/\\beta/&beta/go;
    $text =~ s/\\epsilon/&epsilon/go;
    $text =~ s/\\left\(/\(/go;
    $text =~ s/\\right\)/\)/go;
    $text =~ s/\\R/<FONT FACE=\"Courier New,Courier\"
	COLOR=\"\#666666\"><b>R<\/b><\/FONT>/go;
    $text =~ s/---/&#151/go;
    $text =~ s/--/&#150/go;
    $text =~ s/$EOB/\{/go;
    $text =~ s/$ECB/\}/go;

    $text = replace_command($text, "emph", "<EM>", "</EM>");
    $text = replace_command($text, "bold", "<B>", "</B>");
    $text = replace_command($text, "file", "`<tt>", "</tt>'");

    $text = html_tables($text);
    $text =~ s/\\cr/<BR>/sgo;

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\link")
	  &&  $text =~ /\\link/){
	my ($id, $arg)	= get_arguments("link", $text, 1);
	$htmlfile = $htmlindex{$arg};
	if($htmlfile){
	    $text =~
		s/\\link$id.*$id/<A HREF=\"..\/..\/$htmlfile\">$arg<\/A>/s;
	}
	else{
	    $misslink = $misslink . " " . $arg;
	    $text =~ s/\\link$id.*$id/$arg/s;
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
	$text =~ s/\\deqn(.*)$id/<P align=center><I>$eqn<\/I><\/P>/s;
    }

    $text = replace_command($text, "itemize", "<UL>", "</UL>");
    $text = replace_command($text, "enumerate", "<OL>", "</OL>");

    #$text = replace_command($text, "describe", "<DL>", "</DL>");
    # If `\describe' is used, the  '\item' below will have an argument..
    $text =~ s/\\item\s+/<li>/go;
    # (and be translated to <DL> ..<DD>..

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
	my ($id, $arg)	= get_arguments("link", $text, 1);

	## fix conversions in key of htmlindex:
	my $argkey = $arg;
	$argkey =~ s/&lt;/</go;
	$argkey =~ s/&gt;/>/go;
	$htmlfile = $htmlindex{$argkey};

	if($htmlfile){
	    $text =~
		s/\\link$id.*$id/<A HREF=\"..\/..\/$htmlfile\">$arg<\/A>/s;
	}
	else{
	    $misslink = $misslink . " " . $argkey;
	    $text =~ s/\\link$id.*$id/$arg/s;
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
	print htmlout "<p>\n", text2html($blocks{$block}), "</p>\n";
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
		print htmlout text2html($begin);
		$text =~ s/^$begin//s;
	    }
	    print htmlout "<TABLE>\n";
	    my $loopcount = 0;
	    while(checkloop($loopcount++, $text, "\\item")
		  && $text =~ /\\item/s){
		my ($id, $arg, $desc)  =
		    get_arguments("item", $text, 2);
		print htmlout "<TR VALIGN=TOP><TD><CODE>";
		print htmlout text2html($arg);
		print htmlout "</CODE></TD>\n<TD>\n";
		print htmlout text2html($desc), "</TD></TR>\n";
		$text =~ s/.*$id//s;
	    }
	    print htmlout "</TABLE>\n";
	    print htmlout "<P>\n", text2html($text), "</P>\n";
	}
	else{
	    print htmlout "<P>\n", text2html($text), "</P>\n";
	}
    }
}

# Print sections
sub html_print_sections {

    my $section;

    for($section=0; $section<$max_section; $section++){
	print htmlout html_title3($section_title[$section]);
	print htmlout "<P>\n", text2html($section_body[$section]), "</P>\n";
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



#==************************** nroff ******************************


sub rdoc2nroff { # (filename); 0 for STDOUT

    if($_[0]!= -1) {
      if($_[0]) { open nroffout, "> $_[0]"; } else { open nroffout, "| cat"; }
    }

    $INDENT = "0.5i";
    $TAGOFF = "1i";

    print nroffout ".ND\n";
    print nroffout ".pl 100i\n";
    print nroffout ".po 3\n";
    print nroffout ".na\n";
    print nroffout ".tl '", $blocks{"name"},
          "($pkgname)''R Documentation'\n\n" if $pkgname;
    print nroffout ".SH\n";
    print nroffout $blocks{"title"}, "\n";
    nroff_print_block("description", "Description");
    nroff_print_codeblock("usage", "Usage");
    nroff_print_argblock("arguments", "Arguments");
    nroff_print_block("format", "Format");
    nroff_print_block("details", "Details");
    nroff_print_argblock("value", "Value");

    nroff_print_sections();

    nroff_print_block("note", "Note");
    nroff_print_block("author", "Author(s)");
    nroff_print_block("source", "Source");
    nroff_print_block("references", "References");
    nroff_print_block("seealso", "See Also");
    nroff_print_codeblock("examples", "Examples");

    close nroffout;
}


### Convert a Rdoc text string to nroff
###   $_[0]: text to be converted
###   $_[1]: (optional) indentation of paragraphs. default = $INDENT

sub text2nroff {

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

    ## tables are pre-processed by the tbl(1) command, so this has to
    ## be done first
    $text = nroff_tables($text);
    $text =~ s/\\cr\n?/\n.br\n/sgo;

    $text =~ s/\n\s*\n/\n.IP \"\" $indent\n/sgo;
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
    $text =~ s/---/\\(em/go;
    $text =~ s/--/\\(en/go;
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
	$text =~ s/\\deqn(.*)$id/\n.DS B\n$eqn\n.DE\n/s;
    }

    $list_depth=0;

    $text = replace_command($text,
			    "itemize",
			    "\n.in +$INDENT\n",
			    "\n.in -$INDENT\n");

    $text = replace_command($text,
			    "enumerate",
			    "\n.in +$INDENT\n",
			    "\n.in -$INDENT\n");

    $text =~ s/\\item\s+/\n.ti -\\w\@*\\ \@u\n* /go;

    $text = nroff_unescape_codes($text);
    unmark_brackets($text);
}

sub nroff_parse_lists {

    my $text = $_[0];

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\itemize|\\enumerate") &&
	  $text =~ /\\itemize|\\enumerate/){
	my ($id, $innertext) = get_arguments("deqn", $text, 1);
	if($innertext =~ /\\itemize/){
	    my $tmptext = html_parse_lists($innertext);
	    $text = s/\\itemize$id(.*)$id/\\itemize$id$tmptext$id/s;
	}
	elsif($innertext =~ /\\enumerate/){
	    my $tmptext = html_parse_lists($innertext);
	    $text = s/\\enumerate$id(.*)$id/\\enumerate$id$tmptext$id/s;
	}
	else{
	    if($text =~ /\\itemize|\\enumerate/){
		$text = replace_command($text, "itemize", "<UL>", "</UL>");
		$text = replace_command($text, "enumerate", "<OL>", "</OL>");
		$text =~ s/\\item\s+/<li>/go;
	    }
	}
    }

    $text;
}

sub code2nroff {

    my $text = $_[0];

    $text =~ s/^\.|([\n\(])\./$1\\&./g;
    $text =~ s/\\%/%/go;
    $text =~ s/\\ldots/.../go;
    $text =~ s/\\dots/.../go;
    $text =~ s/\\n/\\\\n/g;

    $text = undefine_command($text, "link");
    $text = undefine_command($text, "dontrun");
    $text = drop_full_command($text, "testonly");

    unmark_brackets($text);
}

# Print a standard block
sub nroff_print_block {

    my ($block,$title) = @_;

    if(defined $blocks{$block}){
	print nroffout "\n";
	print nroffout ".SH\n";
	print nroffout "$title:\n";
#	print nroffout ".LP\n";
#	print nroffout ".in +$INDENT\n";
	print nroffout ".IP \"\" $INDENT\n";
	print nroffout text2nroff($blocks{$block}), "\n";
	print nroffout ".in -$INDENT\n";
    }
}

# Print a code block (preformatted)
sub nroff_print_codeblock {

    my ($block,$title) = @_;

    if(defined $blocks{$block}){
	print nroffout "\n";
	print nroffout ".SH\n" if $title;
	print nroffout "$title:\n" if $title;
	print nroffout ".LP\n";
	print nroffout ".nf\n";
	print nroffout ".in +$INDENT\n";
	print nroffout code2nroff($blocks{$block}), "\n";
	print nroffout ".in -$INDENT\n";
	print nroffout ".fi\n";
    }
}


# Print the value or arguments block
sub nroff_print_argblock {

    my ($block,$title) = @_;

    if(defined $blocks{$block}){

	print nroffout "\n";
	print nroffout ".SH\n" if $title;
	print nroffout "$title:\n" if $title;
#	print nroffout ".LP\n";
#	print nroffout ".in +$INDENT\n";
	print nroffout ".IP \"\" $INDENT\n";

	my $text = $blocks{$block};

	if($text =~ /\\item/s){
	    $text =~ /^(.*)(\\item.*)*/s;
	    my ($begin, $rest) = split(/\\item/, $text, 2);
	    if($begin){
		print nroffout text2nroff($begin);
		$text =~ s/^$begin//s;
	    }
	    my $loopcount = 0;
	    while(checkloop($loopcount++, $text, "\\item") &&
		  $text =~ /\\item/s){
		my ($id, $arg, $desc)  = get_arguments("item", $text, 2);
		$arg = text2nroff($arg);
		$desc = text2nroff($desc);
		print nroffout "\n";
#		print nroffout ".LP\n";
#		print nroffout ".in +$TAGOFF\n";
		print nroffout ".IP \"\" $TAGOFF\n";
		print nroffout ".ti -\\w\@$arg:\\ \@u\n";
		print nroffout "$arg:\\ $desc\n";
#		print nroffout ".in -$TAGOFF\n";
		$text =~ s/.*$id//s;
	    }
	    print nroffout text2nroff($text, $TAGOFF), "\n";
	}
	else{
	    print nroffout text2nroff($text), "\n";
	}
#	print nroffout ".in -$INDENT\n";
    }
}

# Print sections
sub nroff_print_sections {

    my $section;

    for($section=0; $section<$max_section; $section++){
	print nroffout "\n";
	print nroffout ".SH\n";
	print nroffout $section_title[$section], ":\n";
#	print nroffout ".LP\n";
#	print nroffout ".in +$INDENT\n";
	print nroffout ".IP \"\" $INDENT\n";
	print nroffout text2nroff($section_body[$section]), "\n";
#	print nroffout ".in -$INDENT\n";
    }
}


sub nroff_unescape_codes {

    my $text = $_[0];

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "escaped code")
	  && $text =~ /$ECODE($ID)/){
	my $id = $1;
	my $ec = code2nroff($ecodes{$id});
	$text =~ s/$ECODE$id/\`$ec\'/;
    }
    $text;
}


sub nroff_tables {

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

	my $table = ".TS\n";
	for($l=0; $l<$#colformat; $l++){
	    $table .= "$colformat[$l] ";
	}
	$table .= "$colformat[$#colformat].\n";


	# now do the real work: split into lines and columns
	my @rows = split(/\\cr/, $arg);
	for($k=0; $k<=$#rows;$k++){
	    my @cols = split(/\\tab/, $rows[$k]);
	    die("Error:\n  $rows[$k]\\cr\n" .
		"does not fit tabular format \{$format\}\n")
		if ($#cols != $#colformat);
	    for($l=0; $l<$#cols; $l++){
		$table .= "$cols[$l]\t";
	    }
	    $table .= "$cols[$#cols]\n";
	}
	$table .= ".TE\n";

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
	print Sdout ".CS";
	print Sdout text2nroff($blocks{"usage"});
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

    local($tit = $blocks{"title"});

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


sub rdoc2latex {# (filename)

    my($c,$a);

    if($_[0]!= -1) {
      if($_[0]) { open latexout, "> $_[0]"; } else { open latexout, "| cat"; }
    }
    print latexout "\\Header\{";
    print latexout $blocks{"name"};
    print latexout "\}\{";
    print latexout $blocks{"title"};
    print latexout "\}\n";

    foreach (@aliases) {
      $c= code2latex($_,0);
      $a= latex_code_alias($c);
      print STDERR "rdoc2l: alias='$_', code2l(.)='$c', latex_c_a(.)='$a'\n"
	if $debug;
      printf latexout "\\alias\{%s\}\{%s\}\n", $a, $blocks{"name"}
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

    $text =~ s/\\eeeeqn/\\eqn/go;
    $text =~ s/\\dddeqn/\\deqn/og;

    $text =~ s/&/\\&/go;
    $text =~ s/\\R /\\R\\ /go;
    $text =~ s/\\\\/\\bsl{}/go;
    $text =~ s/\\cr/\\\\\{\}/go;
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

    $text =~ s/\\\\/\\bsl{}/go;
    if($hyper) {
	my $loopcount = 0;
	while(checkloop($loopcount++, $text, "\\link")
	      && $text =~ /\\link/) {
	    my ($id, $arg) = get_arguments("link", $text, 1);
	    $text =~ s/\\link$id.*$id/HYPERLINK($arg)/s;
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
	print latexout "\\begin\{$env\}\n";
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
	print latexout code2latex($blocks{$block},0);
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

# Encapsulate code in \verb or \texttt depending on the appearance of
# special characters.
sub latex_code_cmd {

    my $code = $_[0];

    if($code =~ /[$LATEX_SPECIAL]/){
	warn("\nERROR: found `\@' in \\code{...\}\n")
	  if $code =~ /@/;
	warn("\nERROR: found `HYPERLINK(' in \$code: '" . $code ."'\n")
	  if $code =~ /HYPERLINK\(/;
	## till 0.63.1
	## $code = "\\verb@" . $code . "@";
	##          [Problem: Fails in new Methods.Rd: verb NOT in command arg!
	$code =~ s/[$LATEX_SPECIAL]/\\$&/go;# escape them (not the "bsl" )
	$code =~s/\\\^/\$\\,\\hat{\\,}\$/go;# ^ is SPECIAL
	$code =~ s/\\~/\$\\,\\tilde{\\,}\$/go;
    }
    else {
	$code =~ s/HYPERLINK\(([^)]*)\)/\\Link{$1}/go;
    }
    $code = "\\texttt\{" . $code . "\}";
    $code;
}


# Encapsulate code in $...$ by ESCAPING special characters:
# Tough examples are
#	Logic.Rd  Arithmetic.Rd	 Extract.Rd  formula.Rd
sub latex_code_alias {

    my $c = $_[0];  ##-- $c is (typically) the OUTPUT of  code2latex(.) :
    my $BSL = '@BSL@';
    my $Dollar = '@DOLLAR@';

    if($c =~ /[$LATEX_DO_MATH]/){ # (includes LATEX_SPECIAL)
      $c =~ s/\\\\/$BSL/go;
      $c =~ s/\$/$Dollar/go;
      #-- math around it  (should be "robust")
      $c =~ s/[$LATEX_DO_MATH]+/${MD}$&${MD}/g;
      $c =~ s/[$LATEX_SPECIAL]/\\$&/go;	 #- escape them (not the "bsl" \)
      $c =~ s/\|/\\mid{}/go; # "|" is special in '\index' !!
      $c =~ s/\!/\\\!/go;
      $c =~ s/\\\^/\\hat{}/go;# ^ is SPECIAL
      $c =~ s/\\~/\\tilde{}/go;
      $c =~ s/<<-([ ]*)/<\\leftarrow /go;
      $c =~ s/<-([ ]*)/\\leftarrow /go;
      $c =~ s/->([ ]*)/\\rightarrow /go;
      #-
      $c =~ s/$BSL/\\bsl{}/go;
      $c =~ s/$Dollar/\\\$/g;
      $c =~ s/$MD/$Math_del/go;
      }
    $c =~ s/HYPERLINK\(([^)]*)\)/\\Link{$1}/go;
    $c;
}


# Local variables: **
# perl-indent-level: 4 **
# cperl-indent-level: 4 **
# page-delimiter: "^#==" **
# End: **
