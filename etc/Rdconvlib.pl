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
# writing to the Free Software Foundation, Inc., 675 Mass Ave,
# Cambridge, MA 02139, USA.

# Send any bug reports to Friedrich.Leisch@ci.tuwien.ac.at

$VERSION = "0.2.3";#MM
# Bugs: still get ``\bsl{}'' in verbatim-like, see e.g. Examples of apropos.Rd


# names of unique text blocks, these may NOT appear MORE THAN ONCE!
@blocknames = ("name", "title", "usage", "arguments", "description",
	       "value", "references", "seealso", "examples", "author", "note");
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


sub Rdconv {

    open rdfile, "<$_[0]";

    $type = $_[1];
    $debug = $_[2];

    $max_bracket = 0;
    $max_section = 0;

    undef $complete_text;
    undef %blocks;
    undef @section_body;
    undef @section_title;


    #-- remove comments (everything after a %)
    while(<rdfile>){
	next if /^\s*%/o;#- completely drop full comment lines
	my $loopcount = 0;
	while(checkloop($loopcount++, $_, "\\%") &&
	      s/^\\%|([^\\])\\%/$1escaped_percent_sign/go){};
	s/^([^%]*)%.*$/$1/o;
	s/escaped_percent_sign/\\%/go;
	$complete_text = "$complete_text$_";
    }
    printf stderr "-- read file '%s';\n",$_[0] if $debug;

    mark_brackets();
    ##HARD Debug:print "$complete_text\n"; exit;
    escape_codes();
    if($debug) {
      print stderr "\n--------------\nescape codes: '\@ecodes' =\n";

      while(my($id,$code) = each %ecodes) {
	print stderr "\t\$ec{$id}='$code'\n";
      }
    }

    if($type) {
	#-- These may be used in all cases :
	@aliases = get_multi($complete_text,"alias");
	@keywords= get_multi($complete_text,"keyword");
    }

    rdoc2html()	 if $type =~ /html/i;
    rdoc2nroff() if $type =~ /nroff/i;
    rdoc2latex() if $type =~ /tex/i;
    rdoc2ex()	 if $type =~ /example/i;
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

    print stderr "\n-- mark_brackets:" if $debug;
    my $loopcount = 0;
    while(checkloop($loopcount++, $complete_text,
		    "mismatched or missing brackets") &&
	  $complete_text =~ /{([^{}]*)}/s){
	my $id = $NB . ++$max_bracket . $BN;
	$complete_text =~ s/{([^{}]*)}/$id$1$id/s;
	print stderr "." if $debug;
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
#	    return $text;
	    $text =~ s/$id/\{/so;
	}
    }
    $text =~ s/$EOB/\{/gso;
    $text =~ s/$ECB/\}/gso;
    $text;
}

sub escape_codes {
    print stderr "\n-- escape_codes:" if $debug;
    my $loopcount = 0;
    while(checkloop($loopcount++, $complete_text,
		    "while replacing all \\code{...}") &&
	  $complete_text =~ /\\code/){
	my ($id, $arg)	= get_arguments("code", $complete_text, 1);
	$complete_text =~ s/\\code$id(.*)$id/$ECODE$id/s;
	$ecodes{$id} = $1;
	print stderr "," if $debug;
    }
}


# Write documentation blocks such as title, usage, etc. into the
# global hash array %blocks
sub get_blocks {

    my $text = $_[0];

    my $id="";
    print stderr "--- Blocks\n" if $debug;
    foreach $block (@blocknames){
	if($text =~ /\\($block)($ID)/){
	    ($id, $blocks{$block}) = get_arguments($block, $text, 1);
	    print stderr "found: $block\n" if $debug;
	    if((($block =~ /usage/) || ($block =~ /examples/))){
		$blocks{$block} =~ s/^[ \t]+$//; #- multiple empty lines to one
		$blocks{$block} =~ s/\n\n\n/\n\n/gom;
	    } else {
		$blocks{$block} =~ s/^\s*(\S)/$1/;
		$blocks{$block} =~ s/\n[ \t]*(\S)/\n$1/g;
	    }

	    # no formatting commands allowed in the title string
	    if($block =~ /title/) {
		if($blocks{"title"} =~ /$ID/){
		    my $msg = "\nERROR: Environment ";
		    $msg .= "(text enclosed in \{\}) ";
		    $msg .= "found in \\title\{...\}.\n";
		    $msg .= "       The title must be plain text!\n\n";
		    die($msg);
		}
	    }

	}
    }
    print stderr "---\n" if $debug;
}

# Get  ALL  multiblock things -- their simple arg. is put in array:
#
sub get_multi {

    my ($text, $name) = @_; # name: "alias" or "keyword"
    my @res, $k=0;
    print stderr "--- Multi: $name\n" if $debug;
    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\name")
	  && $text =~ /\\$name($ID)/) {
	my $id = $1;
	my ($endid, $arg) =
	    get_arguments($name, $text, 1);
	print stderr "found:" if $debug && $k==0;
	print stderr " $k:$arg" if $debug;
	$arg =~ s/^\s*(\S)/$1/;
	$arg =~ s/\n[ \t]*(\S)/\n$1/g;
	$res[$k++] = $arg;
	$text =~ s/\\$name//s;
    }
    print stderr "\n---\n" if $debug;
    @res;
}

# Write the user defined sections into the
# global hashs @section_body and @section_title
sub get_sections {

    my $text = $_[0];

    print stderr "--- Sections\n" if $debug;
    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\section") &&
	  $text =~ /\\section($ID)/){
	my $id = $1;
	my ($endid, $section, $body)
	    = get_arguments("section", $text, 2);
	print stderr "found: $section\n" if $debug;
	$body =~ s/^\s*(\S)/$1/;
	$body =~ s/\n[ \t]*(\S)/\n$1/g;
	$section_body[$max_section] = $body;
	$section_title[$max_section++] = $section;
	$text =~ s/\\section//s;
    }
    print stderr "---\n" if $debug;
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

	print "\n\n********** $block **********\n\n";
	print $text;
    }
    print "\n";
}



sub undefined_command {

    my ($text, $cmd) = @_;

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\$cmd")
	  &&  $text =~ /\\$cmd/){
	my ($id, $arg)	= get_arguments($cmd, $text, 1);
	$text =~ s/\\$cmd$id(.*)$id/$1/s;
    }
    $text;
}


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


#************************** HTML ********************************


sub rdoc2html {

    get_blocks($complete_text);
    get_sections($complete_text);

    print htmlout "<HTML><HEAD><title>";
    print htmlout $blocks{"title"};
    print htmlout "</title></HEAD><BODY>\n";

    print htmlout "[ <A HREF=\"../../../html/index.html\">top</A>";
    print htmlout " | <A HREF=\"00Index.html\">up</A> ]\n";

    print htmlout "<H2 align=center><I>";
    print htmlout $blocks{"title"};
    print htmlout "</I></H2>\n";

    html_print_codeblock("usage", "Usage");
    html_print_argblock("arguments", "Arguments");
    html_print_block("description", "Description");
    html_print_argblock("value", "Value");

    html_print_sections();

    html_print_block("note", "Note");
    html_print_block("author", "Author(s)");
    html_print_block("references", "References");
    html_print_block("seealso", "See Also");
    html_print_codeblock("examples", "Examples");

    print htmlout "</BODY></HTML>\n";
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
    $text =~ s/\\cr/<BR>/sgo;

    $text =~ s/\\Gamma/&Gamma/go;
    $text =~ s/\\alpha/&alpha/go;
    $text =~ s/\\Alpha/&Alpha/go;
    $text =~ s/\\pi/&pi/go;
    $text =~ s/\\mu/&mu/go;
    $text =~ s/\\sigma/&sigma/go;
    $text =~ s/\\lambda/&lambda/go;
    $text =~ s/\\beta/&beta/go;
    $text =~ s/\\epsilon/&epsilon/go;
    $text =~ s/\\left\(/\(/go;
    $text =~ s/\\right\)/\)/go;
    $text =~ s/$EOB/\{/go;
    $text =~ s/$ECB/\}/go;

    $text = replace_command($text, "emph", "<EM>", "</EM>");
    $text = replace_command($text, "bold", "<B>", "</B>");
    $text = replace_command($text, "file", "`", "'");

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\link")
	  &&  $text =~ /\\link/){
	my ($id, $arg)	= get_arguments("link", $text, 1);
	$htmlfile = $htmlindex{$arg};
	if($htmlfile){
	    $text =~ s/\\link$id.*$id/<A HREF=\"$htmlfile\">$arg<\/A>/s;
	}
	else{
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
	$htmlfile = $htmlindex{$arg};
	if($htmlfile){
	    $text =~ s/\\link$id.*$id/<A HREF=\"$htmlfile\">$arg<\/A>/s;
	}
	else{
	    $text =~ s/\\link$id.*$id/$arg/s;
	}
    }

    $text =~ s/\\\\/\\/go;
    unmark_brackets($text);
}

# Print a standard block
sub html_print_block {

    my ($block,$title) = @_;

    if(defined $blocks{$block}){
	print htmlout "<H3><I>$title</I></H3>\n";
	print htmlout text2html($blocks{$block});
    }
}

# Print a code block (preformatted)
sub html_print_codeblock {

    my ($block,$title) = @_;

    if(defined $blocks{$block}){
	print htmlout "<H3><I>$title</I></H3>\n<PRE>";
	print htmlout code2html($blocks{$block});
	print htmlout "</PRE>";
    }
}


# Print the value or arguments block
sub html_print_argblock {

    my ($block,$title) = @_;

    if(defined $blocks{$block}){
	print htmlout "<H3><I>$title</I></H3>\n";

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
		print htmlout "</CODE>\n<TD>\n";
		print htmlout text2html($desc), "\n";
		$text =~ s/.*$id//s;
	    }
	    print htmlout "</TABLE>\n";
	    print htmlout text2html($text);
	}
	else{
	    print htmlout text2html($text);
	}
    }
}

# Print sections
sub html_print_sections {

    my $section;

    for($section=0; $section<$max_section; $section++){
	print htmlout "<H3><I>" . $section_title[$section] . "</I></H3>\n";
	print htmlout text2html($section_body[$section]);
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


#**************************** nroff ******************************


sub rdoc2nroff {

    get_blocks($complete_text);
    get_sections($complete_text);

    $INDENT = "0.5i";
    $TAGOFF = "1i";

    print nroffout ".ND\n";
    print nroffout ".pl 100i\n";
    print nroffout ".po 3\n";
    print nroffout ".na\n";
    print nroffout ".SH\n";
    print nroffout $blocks{"title"}, "\n";

    nroff_print_codeblock("usage", "");
    nroff_print_argblock("arguments", "Arguments");
    nroff_print_block("description", "Description");
    nroff_print_argblock("value", "Value");

    nroff_print_sections();

    nroff_print_block("note", "Note");
    nroff_print_block("author", "Author(s)");
    nroff_print_block("references", "References");
    nroff_print_block("seealso", "See Also");
    nroff_print_codeblock("examples", "Examples");
}


# Convert a Rdoc text string to nroff
#   $_[0]: text to be converted
#   $_[1]: (optional) indentation of paragraphs. default = $INDENT

sub text2nroff {

    my $text = $_[0];
    if($_[1]){
	my $indent = $_[1];
    }
    else{
	my $indent = $INDENT;
    }

    $text =~ s/^\.|([\n\(])\./$1\\\&./g;

    $text =~ s/\n\s*\n/\n.IP \"\" $indent\n/sgo;
    $text =~ s/\\dots/\\&.../go;
    $text =~ s/\\ldots/\\&.../go;
    $text =~ s/\\cr\n?/\n/sgo;
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
    $text =~ s/\\lambda/lambda/go;
    $text =~ s/\\beta/beta/go;
    $text =~ s/\\epsilon/epsilon/go;
    $text =~ s/\\left\(/\(/go;
    $text =~ s/\\right\)/\)/go;
    $text =~ s/$EOB/\{/go;
    $text =~ s/$ECB/\}/go;

    $text = undefined_command($text, "link");
    $text = undefined_command($text, "emph");
    $text = undefined_command($text, "bold");
    $text = undefined_command($text, "textbf");
    $text = undefined_command($text, "mathbf");
    $text = undefined_command($text, "email");
    $text = replace_command($text, "file", "`", "'");
    $text = replace_command($text, "url", "<URL: ", ">");

    # handle equations:
    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\eqn")
	  &&  $text =~ /\\eqn/){
	my ($id, $eqn, $ascii) = get_arguments("eqn", $text, 2);
	$eqn = $ascii if $ascii;
	$text =~ s/\\eqn(.*)$id/$eqn/s;
    }

    $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\deqn") &&  $text =~ /\\deqn/){
	my ($id, $eqn, $ascii) = get_arguments("deqn", $text, 2);
	$eqn = $ascii if $ascii;
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

    $text = undefined_command($text, "link");

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



#*********************** Example ***********************************


sub rdoc2ex {

    get_blocks($complete_text);

    ##--- Here, I should also put everything which belongs to
    ##--- ./massage-Examples ---- depending on 'name' !!!

    print "###--- >>> `"; print $blocks{"name"};
    print "' <<<----- "; print $blocks{"title"};
    print "\n\n";
    if(@aliases) {
	foreach (@aliases) {
	    print "\t## alias\t help($_)\n";
	}
	print "\n";
    }

    ex_print_exampleblock("examples", "Examples");

    if(@keywords) {
	print "## Keywords: ";
	&print_vec(STDOUT, 'keywords');
    }
    print "\n\n";
}

sub ex_print_exampleblock {

    my ($block,$env) = @_;

    if(defined $blocks{$block}){
	print "##___ Examples ___:\n";
	print  code2examp($blocks{$block});
	print "\n";
    }
}

sub code2examp {
    #-	similar to ..2latex
    my $text = $_[0];

    $text =~ s/\\%/%/go;
    $text =~ s/\\ldots/.../go;
    $text =~ s/\\dots/.../go;

    $text = undefined_command($text, "link");
    $text =~ s/\\\\/\\/g;

    unmark_brackets($text);
}



#*********************** LaTeX ***********************************


sub rdoc2latex {
    my($c,$a);
    get_blocks($complete_text);
    get_sections($complete_text);

    print latexout "\\Header\{";
    print latexout $blocks{"name"};
    print latexout "\}\{";
    print latexout $blocks{"title"};
    print latexout "\}\n";

    foreach (@aliases) {
      $c= code2latex($_,0);
      $a= latex_code_alias($c);
      print stderr "rdoc2l: alias='$_', code2l(.)='$c', latex_c_a(.)='$a'\n"
	if $debug;
      printf latexout "\\alias\{%s\}\{%s\}\n", $a, $blocks{"name"}
        unless /^$blocks{"name"}$/;
    }
    foreach (@keywords) {
      printf latexout "\\keyword\{%s\}\{%s\}\n", $_, $blocks{"name"}
      unless /^$/ ;
    }
    latex_print_codeblock("usage", "Usage");
    latex_print_argblock("arguments", "Arguments");
    latex_print_block("description", "Description");
    latex_print_argblock("value", "Value");

    latex_print_sections();

    latex_print_block("note", "Note");
    latex_print_block("author", "Author");
    latex_print_block("references", "References");
    latex_print_block("seealso", "SeeAlso");
    latex_print_exampleblock("examples", "Examples");

    print latexout "\n";

}

# The basic translator for `normal text'
sub text2latex {

    my $text = $_[0];

    $text =~ s/$EOB/\\\{/go;
    $text =~ s/$ECB/\\\}/go;

    $text =~ s/\\itemize/\\Itemize/o;
    $text =~ s/\\enumerate/\\Enumerate/o;

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


    $text =~ s/\\R /\\R\\ /go;
    $text =~ s/\\\\/\\bsl{}/go;
    $text =~ s/\\cr/\\\\/go;
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
	$text = undefined_command($text, "link");
    }
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
        die("\nERROR: found `\@' in \\code{...\}\n")
	  if $code =~ /@/;
        die("\nERROR: found `HYPERLINK(' in \$code: '" . $code ."'\n")
	  if $code =~ /HYPERLINK\(/;
	$code = "\\verb@" . $code . "@";
    }
    else {
        $code =~ s/HYPERLINK\(([^)]*)\)/\\Link{$1}/go;
	$code = "\\texttt\{" . $code . "\}";
    }
    $code;
}


# Encapsulate code in $...$ by ESCAPING special characters:
# Tough examples are
#	Logic.Rd  Arithmetic.Rd  Extract.Rd  formula.Rd
sub latex_code_alias {

    my $c = $_[0];  ##-- $c is (typically) the OUTPUT of  code2latex(.) :
    my $BSL = '@BSL@';
    my $Dollar = '@DOLLAR@';

    if($c =~ /[$LATEX_DO_MATH]/){ # (includes LATEX_SPECIAL)
      $c =~ s/\\\\/$BSL/go;
      $c =~ s/\$/$Dollar/go;
      #-- math around it  (should be "robust")
      $c =~ s/[$LATEX_DO_MATH]+/${MD}$&${MD}/g;
      $c =~ s/[$LATEX_SPECIAL]/\\$&/go;  #- escape them (not the "bsl" \)
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
