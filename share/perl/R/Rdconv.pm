## Subroutines for converting R documentation into text, HTML, LaTeX and
## R (Examples) format

## Copyright (C) 1997-2003 R Development Core Team
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
## General Public License for more details.
##
## A copy of the GNU General Public License is available via WWW at
## http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
## writing to the Free Software Foundation, Inc., 59 Temple Place, Suite
## 330, Boston, MA 02111-1307 USA.

## Send any bug reports to r-bugs@r-project.org.

package  R::Rdconv;

require  Exporter;
@ISA     = qw(Exporter);
@EXPORT  = qw(Rdconv);

use FileHandle;
use Text::Tabs;
use Text::Wrap;

use R::Utils;
use R::Vars;

if($main::opt_dosnames) { $HTML = ".htm"; } else { $HTML = ".html"; }

## Names of unique text blocks, these may NOT appear MORE THAN ONCE!
@blocknames = ("name", "title", "usage", "arguments", "format",
	       "description", "details", "value", "references",
	       "source", "seealso", "examples", "author", "note",
	       "synopsis", "docType");

## These may appear multiply but are of simple structure:
@multiblocknames = ("alias", "keyword");

## These should NOT contain letters from $LATEX_SPEC
$NB = "normal-bracket";
$BN = "bracket-normal";
$EOB = "escaped-opening-bracket";
$ECB = "escaped-closing-bracket";
$ID = "$NB\\d+$BN";

$EPREFORMAT = "this-is-preformat-code";
$ECODE = "this-is-escaped-code";

$LATEX_SPEC = '\$\^&~_#';#-- these **should** be escaped in  text2latex(.)
$LATEX_SPECIAL = $LATEX_SPEC . '%\{\}\\\\';
$LATEX_DO_MATH = '-+\*/\|<>=!' . $LATEX_SPECIAL;
$MD = ',,,Math,del;;;'; #-- should NOT contain any characters from $LATEX_..
$Math_del = "\$"; #UNquoted '$'
$MAXLOOPS = 10000;

my $EDASH = "escaped-dash";	# maybe something better?
my $ECMD = "escaped-command";	# maybe something better?

## In addition to \code, the following commands are special: dashes in
## their arguments need to be left alone (otherwise, e.g. \samp{--no}
## would give '-no' when converted to text).
my @special_commands = ("command", "env", "file", "kbd", "option",
			"samp", "url", "var");

sub Rdconv { # Rdconv(foobar.Rd, type, debug, filename, pkgname, version)

    $Rdname = $_[0];
    open(rdfile, "<$Rdname") or die "Rdconv(): Couldn't open '$Rdfile': $!\n";

    $type = $_[1];
    $debug = $_[2];
    $pkgname = $_[4];
    $version = $_[5];

    if($type !~ /,/) {
	## Trivial (R 0.62 case): Only 1 $type at a time ==> one
	## filename is ok.
	## filename = 0	  ==>	use stdout
	$htmlfile = $txtfile = $Sdfile = $latexfile = $Exfile =
	  $chmfile = $_[3];
    } else {
	## Have ',' in $type: Multiple types with multiple output files
	$dirname = $_[3]; # The super-directory, such as
                          # '<Rlib>/library/<pkg>'
	die "Rdconv(): '$dirname' is NOT a valid directory: $!\n"
	  unless -d $dirname;
	$htmlfile = file_path($dirname, "html", $Rdname . $HTML)
	  if $type =~ /html/i;
	$txtfile= file_path($dirname, "help", $Rdname)
	  if $type =~ /txt/i;
	die "Rdconv(): type 'Sd' must not be used with other types (',')\n"
	  if $type =~ /Sd/i;
	die "Rdconv(): type 'Ssgm' must not be used with other types (',')\n"
	  if $type =~ /Ssgm/i;
	$latexfile = file_path($dirname, "latex", $Rdname . ".tex")
	  if $type =~ /tex/i;
	$Exfile = file_path($dirname, "R-ex" , $Rdname . ".R")
	  if $type =~ /example/i;
    }

    $max_bracket = 0;
    $max_section = 0;

    undef $complete_text;
    undef %blocks;
    undef @section_body;
    undef @section_title;

    ## Remove comments (everything after a '%') and CR in CRLF
    ## terminators.
    my $skip_level;
    my @skip_state;
    my $skip;
    while(<rdfile>){
	$_ = expand $_;
	s/\r//;
	## <FIXME>
	## Copied from Rdtools::Rdpp() so that nested conditionals are
	## handled correctly.  Should really *call* Rdpp() instead.
	## Known OSdirs are actually ASCII, so this test is OK
	if (/^#ifdef\s+([A-Za-z0-9]+)/o) {
	    $skip = $1 ne $main::OSdir;
            $skip_level += $skip;
            push(@skip_state, $skip);
            next;
	}
	if (/^#ifndef\s+([A-Za-z0-9]+)/o) {
            $skip = $1 eq $main::OSdir;
            $skip_level += $skip;
            push(@skip_state, $skip);
            next;
	}
	if (/^#endif/o) {
            $skip_level -= pop(@skip_state);
            next;
        }
        next if $skip_level > 0;
	next if /^\s*%/o;	# completely drop full comment lines
	my $loopcount = 0;
	while(checkloop($loopcount++, $_, "\\%")
	      && s/^\\%|([^\\])\\%/$1escaped_percent_sign/go) {};
	s/^([^%]*)%.*$/$1/o;
	s/escaped_percent_sign/\\%/go;
	$complete_text .= $_;
    }
    printf STDERR "-- read file '%s';\n",$_[0] if $debug;

    mark_brackets();
    ##HARD Debug:print "$complete_text\n"; exit;
    escape_preformats();
    escape_codes();
    if($debug) {
	print STDERR "\n--------------\nescape codes: '\@ecodes' =\n";

	while(my($id,$code) = each %ecodes) {
	    print STDERR "\t\$ec{$id}='$code'\n";
	}
    }

    if($type) {
	##-- These may be used in all cases :
	@aliases = get_multi($complete_text,"alias");
	@keywords= get_multi($complete_text,"keyword");

	get_blocks($complete_text);

	if($type =~ /html/i || $type =~ /txt/i || $type =~ /Sd/    ||
	   $type =~ /Ssgm/  || $type =~ /tex/i || $type =~ /chm/i ) {

	    get_sections($complete_text);

	} elsif($type =~ /example/i ) {
	    ;
	} else {
	    warn "\n** Rdconv --type '..' : no valid type specified\n";
	}

	rdoc2html($htmlfile)	if $type =~ /html/i;
	rdoc2txt($txtfile)	if $type =~ /txt/i;
	rdoc2Sd($Sdfile)	if $type =~ /Sd/;
	rdoc2Ssgm($Sdfile)	if $type =~ /Ssgm/;
	rdoc2latex($latexfile)	if $type =~ /tex/i;
	rdoc2chm($chmfile)	if $type =~ /chm/i;

	while($text =~ /$EPREFORMAT($ID)/){
	    my $id = $1;
	    my $ec = $epreformat{$id};
	    $text =~ s/$EPREFORMAT$id/$ec/;
	}

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

## Mark each matching opening and closing bracket with a unique id.
## Idea and original code from latex2html
sub mark_brackets {

    $complete_text =~ s/^\\{|([^\\])\\{/$1$EOB/gso;
    $complete_text =~ s/^\\}|([^\\])\\}/$1$ECB/gso;

    print STDERR "\n-- mark_brackets:" if $debug;
    my $loopcount = 0;
    while(checkloop($loopcount++, $complete_text,
		    "mismatched or missing braces")
	  && $complete_text =~ /{([^{}]*)}/s) {
	my $id = $NB . ++$max_bracket . $BN;
	die "too many pairs of braces in this file"
	  if $max_bracket > $MAXLOOPS;
	$complete_text =~ s/{([^{}]*)}/$id$1$id/s;
	print STDERR "." if $debug;
    }
}

sub unmark_brackets {
    my $text = $_[0];

    my $loopcount = 0;
    while(($loopcount++ < $MAXLOOPS)
	  && $text =~ /($ID)(.*)($ID)/s) {
	$id = $1;
	if($text =~ s/$id(.*)$id/\{$1\}/s) {
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
		    "while replacing all \\code{...}")
	  && $complete_text =~ /\\code/) {
	my ($id, $arg) = get_arguments("code", $complete_text, 1);
	$complete_text =~ s/\\code$id(.*)$id/$ECODE$id/s;
	$ecodes{$id} = $1;
	print STDERR "," if $debug;
    }
}

sub escape_preformats {

    print STDERR "\n-- escape_preformats:" if $debug;
    my $loopcount = 0;

    while(checkloop($loopcount++, $complete_text,
		    "while replacing all \\preformatted{...}") &&
	  $complete_text =~ /\\preformatted/  ){
	my ($id, $arg) = get_arguments("preformatted", $complete_text, 1);
	$complete_text =~ s/\\preformatted$id(.*)$id/$EPREFORMAT$id/s;
	my $txt = $1;
	# strip spaces/tabs on last line from Rd formatting in emacs.
	$txt =~ s/[ \t]+$//;
	$epreformats{$id} = $txt;
	$found_any = 1;

	print STDERR "," if $debug;
    }

    $complete_text
}

## Write documentation blocks such as title, usage, etc., into the
## global hash array %blocks.
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

## Get *ALL* multiblock things -- their simple arg. is put in array:
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

## Write the user defined sections into the global hashs @section_body
## and @section_title.
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

## Get the arguments of a command.
sub get_arguments {

    my ($command, $text, $nargs) = @_;
    ## Arguments of get_arguments:
    ##  1, command: next occurence of 'command' is searched
    ##  2, text:    'text' is the text containing the command
    ##  3, nargs:   the optional number of arguments to be extracted;
    ##              default 1
    my @retval;
    ## Returns a list with the id of the last closing bracket and the
    ## arguments.

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

## Get the argument(s) of a link.
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

## Print a short vector of strings (utility).
sub print_vec {
    my($F, $nam, $do_nam, $sep, $end) = @_;
    my($i)=0;
    $sep = ', '	 unless $sep;
    $end = ".\n" unless $end;
    print $F "\@$nam = " if $do_nam;
    foreach (@$nam) { print $F ($i>0 ? $sep : '') . "'$_'"; $i++ }
    print $F $end;
}

## Print the hash %blocks ... for debugging only (I just insert this
## function manually at places where I need it :-)
sub print_blocks {

    while(($block,$text) = each %blocks) {

	print STDERR "\n\n********** $block **********\n\n";
	print STDERR $text;
    }
    print STDERR "\n";
}

## Drop the command and leave its inside argument, i.e., replace
## '\abc{longtext}' by 'longtext'.
sub undefine_command {

    my ($text, $cmd) = @_;

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\$cmd")
	  && $text =~ /\\$cmd(\[[^\]]+\])?$ID/) {
	my ($id, $arg) = get_arguments($cmd, $text, 1);
	$text =~ s/\\$cmd(\[.*\])?$id(.*)$id/$2/s;
    }
    $text;
}

## Drop the command AND its inside argument, i.e., replace
## '_text1_\abc{longtext}-text2-' by '_text1_-text2-'
sub drop_full_command {

    my ($text, $cmd) = @_;
    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\$cmd")
	  && $text =~ /\\$cmd(\[[^\]]+\])?$ID/) {
	my ($id, $arg) = get_arguments($cmd, $text, 1);
	$text =~ s/\\$cmd$id.*$id//s;
    }
    $text;
}

## Replace the command and its closing bracket by $before and $after,
## respectively, e.g., replace '\abc{longtext}' by '<Bef>longtext<Aft>'.
sub replace_command {

    my ($text, $cmd, $before, $after) = @_;

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\$cmd")
	  && $text =~ /\\$cmd(\[[^\]]+\])?$ID/) {
	my ($id, $arg) = get_arguments($cmd, $text, 1);
	$text =~ s/\\$cmd$id(.*)$id/$before$1$after/s;
    }
    $text;
}

# ditto, but add newline before $after unless it starts a new line
# and if there is more than one line.
sub replace_addnl_command {

    my ($text, $cmd, $before, $after) = @_;

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\$cmd")
	  && $text =~ /\\$cmd(\[[^\]]+\])?$ID/) {
	my ($id, $arg) = get_arguments($cmd, $text, 1);
	$text =~ /\\$cmd$id(.*)$id/s;
	$arg = $1;
	if ($arg =~ /\n/m) {
	    $arg = "\n" . $arg unless $arg =~ /^\n/m;
	    $arg = $arg . "\n" unless $arg =~ /\n$/m;
	    $text =~ s/\\$cmd$id(.*)$id/$before$arg$after/s;
	} else {
	    $text =~ s/\\$cmd$id(.*)$id/$before$arg/s;
	}
    }
    $text;
}

## Replace the command and its closing bracket by $before and $after,
## respectively, AND PREPEND a comment to each LINE.  E.g., replace
## '\abc{line1\nline2\n....}' by '<Bef>\n##line1\n##line2\n##....<Aft>'
sub replace_prepend_command {

    my ($text, $cmd, $before, $after, $prepend) = @_;

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\$cmd")
	  && $text =~ /\\$cmd(\[[^\]]+\])?$ID/) {
	my ($id, $arg) = get_arguments($cmd, $text, 1);
	$text =~ /\\$cmd$id(.*)$id/s;
	$arg = $1;
	if ($prepend eq "" || $arg =~ /\n/m) {
	    $arg = "\n" . $arg unless $arg =~ /^\n/m;
	    $arg =~ s/^/$prepend/gmo;# prepend at all line beginnings
	    $arg =~ s/^$prepend//;   # but NOT the very beginning..
	    $arg = $arg . "\n" unless $arg =~ /\n$/m;
	    $text =~ s/\\$cmd$id.*$id/$before$arg$after/s;
	} else {
	    $text =~ s/\\$cmd$id.*$id/$before$arg/s;
	}
    }
    $text;
}

sub transform_command {
    ## Transform the command and its argument.  (Only transforming the
    ## argument causes looping ...)

    my ($text, $cmd, $tcmd, $from, $to) = @_;
    my $loopcount = 0;

    while(checkloop($loopcount++, $text, "\\$cmd") &&
	  $text =~ /\\$cmd(\[[^\]]+\])?$ID/) {
	my ($id, $arg) = get_arguments($cmd, $text, 1);
	$text =~ /\\$cmd$id(.*)$id/s;
	$arg = $1;
	$arg =~ s/$from/$to/g;
	$text =~ s/\\$cmd$id.*$id/\\$tcmd$id$1$arg$id/s;
    }
    $text;
}

sub transform_S3method {
    ## \method{GENERIC}{CLASS}
    ## Note that this markup should really only be used inside \usage.
    ## NB: \w includes _ as well as [:alnum:], which R now allows in name
    my ($text) = @_;
    my $S3method_RE =
      "([ \t]*)\\\\(S3)?method\{([\\w.]+)\}\{([\\w.]+)\}";
    while($text =~ /$S3method_RE/) {
	if($4 eq "default") {
	    $text =~
		s/$S3method_RE/$1\#\# Default S3 method:\n$1$3/s;
	}
	else {
	    $text =~
		s/$S3method_RE/$1\#\# S3 method for class '$4':\n$1$3/s;
	}
    }
    $text;
}

sub transform_S4method {
    ## \S4method{GENERIC}{SIGLIST}
    ## Note that this markup should really only be used inside \usage.
    my ($text) = @_;
    my $S4method_RE =
      "([ \t]*)\\\\S4method\{([\\w.]+)\}\{([\\w.,]+)\}";
    local($Text::Wrap::columns) = 60;
    while($text =~ /$S4method_RE/) {
	my $pretty = wrap("$1\#\# ", "$1\#\#   ",
			  "S4 method for signature '" .
			  join(", ", split(/,/, $3)) . "':\n") .
			  "$1$2";
	$text =~ s/$S4method_RE/$pretty/s;
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

    local $htmlout;
    if($_[0]) {
	$htmlout = new FileHandle;
	open $htmlout, "> $_[0]";  # will be closed when goes out of scope
    } else {
	$htmlout = "STDOUT";
    }
    $using_chm = 0;
    print $htmlout (html_functionhead(html_striptitle($blocks{"title"}),
				      $pkgname, 
				      &html_escape_name($blocks{"name"})));

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

    print $htmlout (html_functionfoot($pkgname, $version));
}

sub html_striptitle {
    ## Call striptitle(), and handle LaTeX single and double quotes.
    my ($text) = @_;
    $text = striptitle($text);
    $text =~ s/\`\`/&ldquo;/g;
    $text =~ s/\'\'/&rdquo;/g;
    $text =~ s/\`/\'/g;		# @samp{'} could be an apostroph ...
    $text;
}

sub html_escape_name {
    my ($text) = @_;
    $text = unmark_brackets($text);
    $text =~ s/\\%/%/g;
    $text =~ s/\\\\/\\/g;
    $text;
}



## Convert a Rdoc text string to HTML, i.e., convert \code to <tt> etc.
sub text2html {

    my $text = $_[0];
    my $outerpass = $_[1];
    my $inarglist = $_[2];

    if($outerpass) {
        $text =~ s/&([^#])/&amp;\1/go; # might have explicit &# in source
	$text =~ s/>/&gt;/go;
	$text =~ s/</&lt;/go;
	$text =~ s/\\%/%/go;

	if($inarglist) {
	    $text =~ s/\n\s*\n/\n<br>\n/sgo;
	} else {
	    $text =~ s/\n\s*\n/\n<\/p>\n<p>\n/sgo;
	}
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
	$text =~ s/\\le/&lt;=/go;# \le *after* \left !
	$text =~ s/\\ge/&gt;=/go;
	$text =~ s/\\R/<font face=\"Courier New,Courier\" color=\"\#666666\"><b>R<\/b><\/font>/go;
	foreach my $cmd (@special_commands) {
	    $text = transform_command($text, $cmd, $ECMD . $cmd,
				      "-", "$EDASH");
	}
	## <FIXME>
	## Can we safely assume HTML 4 these days?
	## (HTML 4.0 Specification last revised on 24-Apr-1998)
	## See also below for single/double left/right quotes.
	## $text =~ s/---/&#151;/go;
	$text =~ s/---/&mdash;/go;
	## $text =~ s/--/&#150;/go;
	$text =~ s/--/&ndash;/go;
	## <FIXME>
	foreach my $cmd (@special_commands) {
	    $text = transform_command($text, $ECMD . $cmd, $cmd,
				      "$EDASH", "-");
	}
	$text =~ s/$EOB/\{/go;
	$text =~ s/$ECB/\}/go;
    }

    $text = replace_command($text, "emph", "<EM>", "</EM>");
    $text = replace_command($text, "bold", "<B>", "</B>");
    $text = replace_command($text, "file", "&lsquo;<TT>", "</TT>&rsquo;");

    $text = replace_command($text, "strong", "<STRONG>", "</STRONG>");

    $text = replace_command($text, "acronym", "<SMALL>", "</SMALL>");
    $text = replace_command($text, "cite", "<CITE>", "</CITE>");
    $text = replace_command($text, "command", "<CODE>", "</CODE>");
    $text = replace_command($text, "dfn", "<DFN>", "</DFN>");
    $text = replace_command($text, "env", "<CODE>", "</CODE>");
    $text = replace_command($text, "kbd", "<KBD>", "</KBD>");
    $text = replace_command($text, "option", "<SAMP>", "</SAMP>");
    $text = replace_command($text, "pkg", "<STRONG>", "</STRONG>");
    $text = replace_command($text, "samp", "<SAMP>", "</SAMP>");
    $text = replace_command($text, "var", "<VAR>", "</VAR>");

    $text = replace_command($text, "sQuote", "&lsquo;", "&rsquo;");
    $text = replace_command($text, "dQuote", "&ldquo;", "&rdquo;");

    $text = html_tables($text);
    $text =~ s/\\cr/<br>/sgo;

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\link")
	  &&  $text =~ /\\link/){
	my ($id, $arg, $opt) = get_link($text);
	## fix conversions in key of htmlindex:
	my $argkey = $arg;
	$argkey =~ s/&lt;/</go;
	$argkey =~ s/&gt;/>/go;
	die "\nERROR: command (e.g. \\url) inside \\link\n" 
	    if $arg =~ normal-bracket;
	$htmlfile = $main::htmlindex{$argkey};
	if($htmlfile && !length($opt)){
	    if($using_chm) {
		if ($htmlfile =~ s+^$pkgname/html/++) {
		    # in the same chm file
		    $text =~
			s/\\link(\[.*\])?$id.*$id/<a href=\"$htmlfile\">$arg<\/a>/s;
		} else {
		    $tmp = $htmlfile;
		    ($base, $topic) = ($tmp =~ m+(.*)/(.*)+);
		    $base =~ s+/html$++;
		    $htmlfile = mklink($base, $topic);
#		    print "$htmlfile\n";
		    $text =~
			s/\\link(\[.*\])?$id.*$id/<a href=\"$htmlfile\">$arg<\/a>/s;
		}
	    } else {
		if ($htmlfile =~ s+^$pkgname/html/++) {
		    # in the same html file
		    $text =~
			s/\\link(\[.*\])?$id.*$id/<a href=\"$htmlfile\">$arg<\/a>/s;
		} elsif ($htmlfile =~ s+^$pkgname\_[^/]*/html/++) {
		    # in the same html file, versioned install
		    $text =~
			s/\\link(\[.*\])?$id.*$id/<a href=\"$htmlfile\">$arg<\/a>/s;
		} else {
		    $text =~
			s/\\link(\[.*\])?$id.*$id/<a href=\"..\/..\/$htmlfile\">$arg<\/a>/s;
		}
	    }
	}
	else {
	    $main::misslink = $main::misslink . " " . $argkey
		unless $opt ne "";
	    if($using_chm){
		if($opt ne "") {
		    my ($pkg, $topic) = split(/:/, $opt);
		    $topic = $arg if $topic eq "";
		    $opt =~ s/:.*$//o;
		    if($pkg ne $pkgname) {
			$htmlfile = mklink($opt, $topic . $HTML);
		    } else {
			$htmlfile = $topic . $HTML;
		    }
		    $text =~ s/\\link(\[.*\])?$id.*$id/<a $htmlfile>$arg<\/a>/s;
		} else {
		    $text =~ s/\\link(\[.*\])?$id.*$id/$arg/s;
		}
	    }
	    else{
		if($opt ne "") {
		    my ($pkg, $topic) = split(/:/, $opt);
		    $topic = $arg if $topic eq "";
		    $htmlfile = $pkg."/html/".$topic.$HTML;
		    if ($htmlfile =~ s+^$pkgname/html/++) {
			# in the same html file
			$text =~ s/\\link(\[.*\])?$id.*$id/<a href=\"$htmlfile\">$arg<\/a>/s;
		    } elsif ($htmlfile =~ s+^$pkgname\_[^/]*/html/++) {
			# in the same html file, versioned install
			$text =~ s/\\link(\[.*\])?$id.*$id/<a href=\"$htmlfile\">$arg<\/a>/s;
		    } else {
			$text =~ s/\\link(\[.*\])?$id.*$id/<a href=\"..\/..\/$htmlfile\">$arg<\/a>/s;
		    }
		} else {
		    $text =~ s/\\link(\[.*\])?$id.*$id/<a href=\"..\/..\/..\/doc\/html\/search\/SearchObject.html?$argkey\">$arg<\/a>/s;
		}
	    }
	}
    }

    $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\email")
	  &&  $text =~ /\\email/){
	my ($id, $arg)	= get_arguments("email", $text, 1);
	$text =~ s/\\email$id.*$id/<a href=\"mailto:$arg\">$arg<\/a>/s;
    }

    $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\url")
	  &&  $text =~ /\\url/){
	my ($id, $arg)	= get_arguments("url", $text, 1);
	$text =~ s/\\url.*$id/<a href=\"$arg\">$arg<\/a>/s;
    }

    ## Handle equations:
    $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\eqn")
	  &&  $text =~ /\\eqn/){
	my ($id, $eqn, $ascii) = get_arguments("eqn", $text, 2);
	$eqn = $ascii if $ascii;
	$text =~ s/\\eqn(.*)$id/<i>$eqn<\/i>/s;
    }

    $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\deqn")
	  &&  $text =~ /\\deqn/){
	my ($id, $eqn, $ascii) = get_arguments("deqn", $text, 2);
	$eqn = $ascii if $ascii;
	$text =~ s/\\deqn(.*)$id/<\/p><p align="center"><i>$eqn<\/i><\/p><p>/s;
    }

    $text = replace_command($text, "itemize", "<ul>", "</ul>");
    $text = replace_command($text, "enumerate", "<ol>", "</ol>");
    $text =~ s/<\/p>\n<p>\s+\\item\s+/<li>/go;
    $text =~ s/\\item\s+/<li>/go;

    ## Handle '\describe':
    $text = replace_command($text, "describe", "<dl>", "</dl>");
    while(checkloop($loopcount++, $text, "\\item")
	  && $text =~ /\\itemnormal/s) {
	my ($id, $arg, $desc)  = get_arguments("item", $text, 2);
	my $descitem;
	$descitem = "<dt>" . text2html($arg, 0, $inarglist) . "</dt>";
	$descitem .= "<dd>" . text2html($desc, 0, $inarglist) . "</dd>";
	$text =~ s/\\itemnormal.*$id/$descitem/s;
    }
    if($outerpass) {
	$text =~ s/\\([^\\])/$1/go; #-drop single "\" (as in '\R')
	$text =~ s/\\\\/\\/go;
	$text = html_unescape_codes($text);
	$text = unmark_brackets($text);
    }
    $text;
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
	$argkey =~ s/&amp;/&/go;
	$htmlfile = $main::htmlindex{$argkey};

	if($htmlfile && !length($opt)){
	    if($using_chm) {
		if ($htmlfile =~ s+^$pkgname/html/++) {
		    # in the same chm file
		    $text =~
			s/\\link(\[.*\])?$id.*$id/<a href=\"$htmlfile\">$arg<\/a>/s;
		} else {
		    $tmp = $htmlfile;
		    ($base, $topic) = ($tmp =~ m+(.*)/(.*)+);
		    $base =~ s+/html$++;
		    $htmlfile = mklink($base, $topic);
		    $text =~
			s/\\link(\[.*\])?$id.*$id/<a $htmlfile>$arg<\/a>/s;
		}
	    } else {
		my $uxfile = $htmlfile;
		if ($uxfile =~ s+^$pkgname/html/++) {
		    # in the same html file
		    $text =~
			s/\\link(\[.*\])?$id.*$id/<a href=\"$uxfile\">$arg<\/a>/s;
		} elsif ($uxfile =~ s+^$pkgname\_[^/]*/html/++) {
		    # in the same html file, versioned install
		    $text =~
			s/\\link(\[.*\])?$id.*$id/<a href=\"$uxfile\">$arg<\/a>/s;
		} else {
		    $text =~
			s/\\link(\[.*\])?$id.*$id/<a href=\"..\/..\/$uxfile\">$arg<\/a>/s;
		}
	    }
	}
	else{
	    $main::misslink = $main::misslink . " " . $argkey
		unless $opt ne "";
	    if($using_chm){
		if($opt ne "") {
		    my ($pkg, $topic) = split(/:/, $opt);
		    $topic = $arg if $topic eq "";
		    $opt =~ s/:.*$//o;
		    if($pkg ne $pkgname) {
			$htmlfile = mklink($opt, $topic . $HTML);
		    } else {
			$htmlfile = $topic . $HTML;
		    }
       		    $text =~ s/\\link(\[.*\])?$id.*$id/<a $htmlfile>$arg<\/a>/s;
		} else {
		    $text =~ s/\\link(\[.*\])?$id.*$id/$arg/s;
		}
	    } else {
		if($opt ne "") {
		    my ($pkg, $topic) = split(/:/, $opt);
		    $topic = $arg if $topic eq "";
		    $htmlfile = $pkg."/html/".$topic.$HTML;
		    if ($htmlfile =~ s+^$pkgname/html/++) {
			# in the same html file
			$text =~
			    s/\\link(\[.*\])?$id.*$id/<a href=\"$htmlfile\">$arg<\/a>/s;
		    } elsif ($htmlfile =~ s+^$pkgname\_[^/]*/html/++) {
			# in the same html file, versioned install
			$text =~
			    s/\\link(\[.*\])?$id.*$id/<a href=\"$htmlfile\">$arg<\/a>/s;
		    } else {
			$text =~ s/\\link(\[.*\])?$id.*$id/<a href=\"..\/..\/$htmlfile\">$arg<\/a>/s;
		    }
		} else {
		    $text =~ s/\\link(\[.*\])?$id.*$id/<a href=\"..\/..\/..\/doc\/html\/search\/SearchObject.html?$argkey\">$arg<\/a>/s;
		}
	    }
	}
    }

    $text = replace_addnl_command($text, "dontrun",
				  "## Not run: ", "## End(Not run)");
    $text = drop_full_command($text, "testonly");
    $text = drop_full_command($text, "dontshow");
    $text =~ s/\\\\/\\/go;

    $text = unmark_brackets($text);

    $text = transform_S3method($text);
    $text = transform_S4method($text);

    $text;
}

## Print a standard block
sub html_print_block {

    my ($block,$title) = @_;

    html_print_a_section($title, $blocks{$block})
      if defined $blocks{$block};
}

## Print a code block (preformatted)
sub html_print_codeblock {

    my ($block,$title) = @_;

    if(defined $blocks{$block}){
	print $htmlout (html_title3($title), "<pre>" ,
			code2html($blocks{$block}), "</pre>\n\n");
    }
}

## Print the value or arguments block
sub html_print_argblock {

    my ($block,$title) = @_;

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "escaped preformat")
	  && $text =~ /$EPREFORMAT($ID)/){
	my $id = $1;
	my $ec = code2html($epreformats{$id});
	$text =~ s/$EPREFORMAT$id/<pre>$ec<\/pre>/;
    }

    if(defined $blocks{$block}){
	print $htmlout (html_title3($title));

	my $text = $blocks{$block};

	if($text =~ /\\item/s){
	    $text =~ /^(.*)(\\item.*)*/s;
	    my ($begin, $rest) = split(/\\item/, $text, 2);
	    if($begin){
		$text =~ s/^$begin//s;
		$begin =~ s/(\n)+$//;
		print $htmlout "<p>\n", text2html($begin, 1, 1), "\n</p>\n";
	    }
	    print $htmlout "<table summary=\"R argblock\">\n";
	    my $loopcount = 0;
	    while(checkloop($loopcount++, $text, "\\item")
		  && $text =~ /\\item/s) {
		my ($id, $arg, $desc)  =
		    get_arguments("item", $text, 2);
		print $htmlout ("<tr valign=\"top\"><td><code>",
				text2html($arg, 1, 1),
				"</code></td>\n<td>\n",
				text2html($desc, 1, 1), "</td></tr>\n");
		$text =~ s/.*$id//s;
	    }
	    print $htmlout "</table>\n";
	    my $rest = text2html($text, 1, 1);
	    print $htmlout ("<p>\n", $rest, "</p>\n") if $rest;
	}
	else{
	    my $rest = text2html($text, 1, 1);
	    print $htmlout ("<p>\n", $rest, "</p>\n") if $rest;
	}
    }
}

## Print sections
sub html_print_sections {

    my $section;

    for($section=0; $section<$max_section; $section++){
	html_print_a_section(html_striptitle($section_title[$section]),
			     $section_body[$section]);
    }
}

sub html_print_a_section {
    my ($title, $body) = @_;
    my $htmlbody = text2html($body, 1, 0);

    $htmlbody =~ s/<p>\s*<p/<p/g;  # before deqn
    $htmlbody =~ s/<\/p>\s*<\/p>/<\/p>/g;
    ## attempt to close paragraphs tags, and remove spurious closings.
    ## next one gets thrown by the unclosed <li> tags.
    ##    $htmlbody =~ s/([^>]\n+)<(table|dl|ul|ol)/\1<\/p>\n<\2/g;
    $htmlbody =~ s/<\/(table|dl|ul|ol|dd)>\n+<\/p>\n/<\/\1>\n\n/g;
    $htmlbody =~ s/<\/(table|dl|ul|ol)>\n+(\w|<em|<code|<b)/<\/\1>\n<p>\n\2/g;
    $htmlbody =~ s/<p>\s*<(table|dl|ul|ol|dt)/\n<\1/g;

    ## top and tail with paragraph tags if needed.
    $htmlbody = "<p>\n". $htmlbody unless $htmlbody =~ /^<(table|dl|ul|ol)>/;
    $htmlbody .= "\n</p>\n" unless $htmlbody =~ /<\/(table|dl|ul|ol)>\s*$/;

    ## remove empty paras
    $htmlbody =~ s/<p>\s*<\/p>//g;

    print $htmlout (html_title3($title), $htmlbody, "\n");
}

sub html_unescape_codes {

    my $text = $_[0];

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "escaped code")
	  && $text =~ /$ECODE($ID)/) {
	my $id = $1;
	my $ec = code2html($ecodes{$id});
	$text =~ s/$ECODE$id/<code>$ec<\/code>/;
    }

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "escaped preformat")
	  && $text =~ /$EPREFORMAT($ID)/){
	my $id = $1;
	my $ec = code2html($epreformats{$id});
	$text =~ s/$EPREFORMAT$id/<pre>$ec<\/pre>/;
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

	## remove trailing \cr (otherwise we get an empty last line)
	$arg =~ s/\\cr\s*$//go;

	## parse the format of the tabular environment
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

	## now do the real work: split into lines and columns
	my $table = "<table summary=\"Rd table\">\n";
	my @rows = split(/\\cr/, $arg);
	for($k=0; $k<=$#rows;$k++){
	    $table .= "<tr>\n";
	    my @cols = split(/\\tab/, $rows[$k]);
	    die("Error:\n  $rows[$k]\\cr\n" .
		"does not fit tabular format \{$format\}\n")
		if ($#cols != $#colformat);
	    for($l=0; $l<=$#cols; $l++){
		$table .= "  <td align=\"$colformat[$l]\">$cols[$l]</td>";
	    }
	    $table .= "\n</tr>\n";
	}
	$table .= "</table>\n";
	$text =~ s/\\tabular.*$id/$table/s;
    }

    $text;
}

sub html_title3
{
    my $title = $_[0];

    "\n<h3>$title</h3>\n\n";
}

## The header & footer of a function page

sub html_functionhead
{
    my ($title, $pkgname, $name) = @_;

    my $retval = "<html><head><title>R: $title</title>\n" .
	"<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">\n" .
	"<link rel=\"stylesheet\" type=\"text/css\" href=\"../../R.css\">\n" .
	"</head><body>\n\n";

    if($pkgname){
	$retval .= "<table width=\"100%\" summary=\"page for $name {$pkgname}\"><tr>" .
	    "<td>$name {$pkgname}</td>" .
	    "<td align=\"right\">R Documentation</td></tr></table>";
    }

    $retval .= "\n<h2>$title</h2>\n\n";
}

sub html_functionfoot
{
    my ($pkgname, $version) = @_;
    my $retval;

    if($HTML){
	$retval .= "\n\n<hr><div align=\"center\">[Package";
 	$retval .= " <em>$pkgname</em>" if $pkgname ne "unknown";
	$retval .= " version $version" if $version ne "";
	$retval .= " <a href=\"00Index$HTML\">Index]</a></div>\n\n";
    }

    $retval .= "</body></html>\n";
}

sub chm_functionhead
{
    my ($title, $pkgname, $name) = @_;

    my $retval = "<html><head><title>$title</title>\n" .
	"<link rel=\"stylesheet\" type=\"text/css\" href=\"Rchm.css\">\n".
	    "</head>\n<body>\n\n";

    if($pkgname){
	$retval .= "<table width=\"100%\"><tr>" .
	    "<td>$name($pkgname)</td>" .
	    "<td align=\"right\">R Documentation</td></tr></table>";
    }

    $retval .= "<object type=\"application/x-oleobject\" classid=\"clsid:1e2a7bd0-dab9-11d0-b93a-00c04fc99f9e\">\n";
    foreach(@aliases) {
#	print "alias: $_\n";
	$retval .= "<param name=\"keyword\" value=\"R:   $_\">\n";
    }
    $title =~ s/\"/'/go;  #'
    $title =~ s/,//go;  # commas seem to give problems
    $retval .= "<param name=\"keyword\" value=\" $title\">\n" .
	"</object>\n\n";
    $retval .= "\n<h2>$title</h2>\n\n";
}


#==************************** txt ******************************

use Text::Tabs qw(expand);

sub rdoc2txt { # (filename); 0 for STDOUT

    local $txtout;
    if($_[0]) {
	$txtout = new FileHandle;
	open $txtout, "> $_[0]";  # will be closed when goes out of scope
    } else {
	$txtout = "STDOUT";
    }

    $INDENT = 3;  # indent for \itemize and \enumerate first line
    $INDENTD = 0; # indent for \describe list first line
    $INDENTDD = 5; # indent for \describe list bodies

    if ($pkgname) {
	my $pad = 75 - length($blocks{"name"}) - length($pkgname) - 30;
	$pad = int($pad/2);
	print $txtout  &html_escape_name($blocks{"name"}), " " x $pad,
	"package:$pkgname", " " x $pad,"R Documentation\n\n";
    }
    print $txtout (txt_header(txt_striptitle($blocks{"title"})), "\n");
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

    print $txtout "\n";
    if($_[0]) { close $txtout; }
}

sub txt_striptitle {
    ## Call striptitle(), and handle LaTeX style single/double quotes.
    my ($text) = @_;
    $text = striptitle($text);
    $text =~ s/(\`\`|\'\')/\"/g;
    $text =~ s/\`/\'/g;
    $text;
}

## Underline section headers
sub txt_header {

    my $header = $_[0];
    $header =~ s/\\//go;
    ##    '_' . join '_', split //, $header;
    my @letters = split //, $header;
    my $out = "", $a;
    for($l = 0; $l <= $#letters; $l++){
	$a = @letters[$l];
	if($a =~ /[[:alnum:]]/) {
	    $out .= '_' . $a;
	} else {
	    $out .= $a;
	}
    }
    return $out;
}

## Convert a Rdoc text string to txt
##   $_[0]: text to be converted
##   $_[1]: (optional) indentation of paragraphs. default = $INDENT

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
    $text =~ s/\\le/<=/go;
    $text =~ s/\\ge/>=/go;
    $text =~ s/\\R/R/go;

    foreach my $cmd (@special_commands) {
	$text = transform_command($text, $cmd, $ECMD . $cmd,
				  "-", "$EDASH");
    }
    $text =~ s/---/--/go;
    $text =~ s/--/-/go;
    foreach my $cmd (@special_commands) {
	$text = transform_command($text, $ECMD . $cmd, $cmd,
				  "$EDASH", "-");
    }
    $text =~ s/$EOB/\{/go;
    $text =~ s/$ECB/\}/go;

    $text = undefine_command($text, "link");
    $text = undefine_command($text, "textbf");
    $text = undefine_command($text, "mathbf");
    $text = undefine_command($text, "email");

    $text = replace_command($text, "file", "'", "'");
    $text = replace_command($text, "url", "<URL: ", ">");

    $text = replace_command($text, "emph", "_", "_");
    $text = replace_command($text, "bold", "*", "*");
    $text = replace_command($text, "strong", "*", "*");

    $text = undefine_command($text, "acronym");
    $text = undefine_command($text, "cite");
    $text = undefine_command($text, "dfn");

    $text = replace_command($text, "command", "'", "'");
    $text = replace_command($text, "env", "'", "'");
    $text = replace_command($text, "kbd", "'", "'");
    $text = replace_command($text, "option", "'", "'");
    $text = replace_command($text, "pkg", "'", "'");
    $text = replace_command($text, "samp", "'", "'");

    ## <FIXME>
    ## Maybe this should uppercase its argument a la Texinfo?
    $text = undefine_command($text, "var");
    ## </FIXME>

    $text = replace_command($text, "sQuote", "'", "'");
    $text = replace_command($text, "dQuote", "\"", "\"");

    ## Handle equations:
    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\eqn")
	  &&  $text =~ /\\eqn/){
	my ($id, $eqn, $ascii) = get_arguments("eqn", $text, 2);
	$eqn = $ascii if $ascii;
	$eqn =~ s/\\([^&])/$1/go;
	$text =~ s/\\eqn(.*)$id/$eqn/s;
    }

    $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\deqn")
	  && $text =~ /\\deqn/) {
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

    ## Handle '\describe':
    $text = replace_command($text,
			    "describe",
			    "\n.in +$INDENTDD\n",
			    "\n.in -$INDENTDD\n");
    while(checkloop($loopcount++, $text, "\\item")
	  && $text =~ /\\itemnormal/s) {
	my ($id, $arg, $desc)  = get_arguments("item", $text, 2);
	my $descitem = text2txt($arg);
	my $ll = length($desc);
	$descitem =~ s/\n/ /go;  # no NLs in items
	if($ll > 0) {
	    $descitem = "\n.tide " . $descitem . " \n". text2txt($desc);
	} else {
	    warn "Warning: missing text for item '$descitem' " .
		"in \\describe\n";
	    $descitem = "\n.tide " . $descitem . " \n \n"
	}
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
    $text = replace_addnl_command($text, "dontrun",
				  "## Not run: ", "## End(Not run)");
    $text = drop_full_command($text, "testonly");
    $text = drop_full_command($text, "dontshow");

    $text = unmark_brackets($text);

    $text = transform_S3method($text);
    $text = transform_S4method($text);

    $text;
}


sub nounder
{
    my ($text) = @_;
    $text =~ s/_//g;
    $text;
}

## Modified from Text::Wrap to take account of underlines, not entab.
sub Rwrap
{
    my ($ip, $xp, @t) = @_;

    my $r = "";
    my $columns = 72;
    my $t = expand(join(" ",@t));
    my $lead = $ip;
    my $ll = $columns - length(nounder(expand($ip))) - 1;
    my $nll = $columns - length(nounder(expand($xp))) - 1;
    my $nl = "";
    my $remainder = "";

    if ($ll <= 0) {
	##	warn "warning. indent:\n".
	##	    &nounder(expand($ip))."\nis wider than the page\n";
	$ll = 5;
    }
    while ($t !~ /^\s*$/) {
	if ($t =~ s/^([^\n]{0,$ll})(\s|\Z(?!\n))//xm) {
	    $r .= $nl . $lead . $1;
	    $remainder = $2;
	} elsif ($t =~ s/^([^\n]{$ll})//) {
	    $r .= $nl . $lead . $1;
	    $remainder = "\n";
	} else {
	    print "$t\n";
	    die "This shouldn't happen";
	}

	$lead = $xp;
	$ll = $nll;
	$nl = "\n";
    }
    $r .= $remainder;

    $r .= $lead . $t if $t ne "";

    return $r;
}

## generate wrapped text, zap empty lines to \n
sub mywrap {
    my ($pre1, $pre2, $text) = @_;

    my $ntext;
    if(length($text) > 0) {
	$ntext = Rwrap($pre1, $pre2, $text);
    } else {
	$ntext = $pre1;
    }
    my @lines = split /\n/, $ntext;
    my $out = "", $line;
    foreach $line (@lines) {
	##	$line = expand $line;
	$line =~ s/^\s+$//o;
	$out .= $line . "\n";
    }
    $out =~ s/\n$//;
    return $out;
}

## Print text indent and filled: will put out a leading blank line.
sub txt_fill { # pre1, base, "text to be formatted"
    my ($pre1, $base, $text) = @_;
    my $INDENT = $base;
    my $indent = " " x $INDENT;

    ## first split by paragraphs

    $text =~ s/\\\\/\\bsl{}/go;
    $text =~ s/\\&\./\./go; # unescape code pieces
    ## A mess:  map  & \& \\& \\\& to  & & \& \&
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
	## strip leading white space
	$para  =~ s/^\s+//;
	my $para0 = $para;
	$para0 =~ s/\n\s*/ /go;
	## check for a item in itemize etc
	if ($para =~ s/^[\n]*\.ti //) {
	    $indent1 = $indent;
	    $indent2 = $indent1 . (" " x 3);
	    if ($enum{$enumlevel} > 0) {
		$para =~ s/\*/$enum{$enumlevel}./;
		$enum{$enumlevel} += 1;
	    }
	}
	## check for a item in describe etc
	if ($para =~ s/^[\n]*\.tide ([^\n]+)\n//) {
	    $indent1 = " " x ($INDENT - $INDENTDD) . txt_header($1);
	    $indent2 = $indent;
	}
        ## check for .in or .inen command
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
        ## check for a \deqn block
	} elsif ($para0 =~ s/^\s*\.DS B\s*(.*)\.DE/\1/) {
	    $para0 =~ s/\s*$//o;
	    if(length($para0) > 65) {
		print $txtout "\n", " ", $para0, "\n";
	    } else {
		my $shift = int((70 - length($para0))/2);
		print $txtout "\n", " " x $shift, $para0, "\n";
	    }

	## check for a \tabular block
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
	    print $txtout "\n";
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
		print $txtout $indent, "$line\n";
	    }

	## plain text
	} else {
	    $para =~ s/\n\s*/ /go;
	    print $txtout "\n";
	    # Now split by \cr blocks
	    my @blocks = split /\\cr/, $para;
	    foreach $text (@blocks) {
#		$text =~ s/^\s+//o;
		print $txtout mywrap($indent1, $indent2, $text), "\n";
		$indent1 = $indent2;
	    }
	}
    }
}

## Print a standard block
sub txt_print_block {

    my ($block,$title) = @_;
    my $next;

    if(defined $blocks{$block}){
	print $txtout "\n";
	print $txtout txt_header($title), ":\n";
	$ntext = text2txt($blocks{$block});
	txt_fill("     ", 5, $ntext);
    }
}

## Print a code block (preformatted)
sub txt_print_codeblock {

    my ($block,$title) = @_;
    my $ntext;
    my $indent = " " x 5;

    if(defined $blocks{$block}){
	print $txtout "\n";
	print $txtout txt_header($title), ":\n" if $title;
	$ntext = code2txt($blocks{$block});
	# make sure there is precisely one leading "\n"
	$ntext =~ s/^[\n]*//go;
	$ntext = "\n". $ntext;
	$ntext =~ s/\\&\././go;
	foreach $line (split /\n/, $ntext) {
	    $line =~ s/\\\\/\\/go;
	    $line =~ s/^\t/        /o;
#	    $line =~ s/^\s+$//o;
	    if(length($line) > 0) {
		print $txtout $indent, $line, "\n";
	    } else {
		print $txtout "\n";
	    }
	}
    }
}


## Print the value or arguments block
sub txt_print_argblock {

    my ($block,$title) = @_;

    if(defined $blocks{$block}){

	print $txtout "\n";
	print $txtout txt_header($title), ":\n" if $title;

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
		    print $txtout "\n", $arg0, "\n";
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

## Print sections
sub txt_print_sections {

    my $section;

    for($section=0; $section<$max_section; $section++){
	print $txtout "\n";
	print $txtout txt_header(txt_striptitle($section_title[$section])), ":\n";
	txt_fill("     ", 5, text2txt($section_body[$section]));
    }
}


sub txt_unescape_codes {

    my $text = $_[0];

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "escaped code")
	  && $text =~ /$ECODE($ID)/) {
	my $id = $1;
	my $ec = code2txt($ecodes{$id});
	$text =~ s/$ECODE$id/\'$ec\'/;
    }

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "escaped preformat")
	  && $text =~ /$EPREFORMAT($ID)/){
	my $id = $1;
	my $ec = code2txt($epreformats{$id});
        $ec =~ s/\n/\\cr/g;
	$text =~ s/$EPREFORMAT$id/\n$ec\n/;
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

	## remove trailing \cr (otherwise we get an empty last line)
	$arg =~ s/\\cr\s*$//go;

	## parse the format of the tabular environment
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

    local $Sdout;
    if($_[0]) {
	$Sdout = new FileHandle;
	open $Sdout, "> $_[0]";  # will be closed when goes out of scope
    } else {
	$Sdout = "STDOUT";
    }

    print $Sdout "\.\\\" -*- nroff -*- generated from \.Rd format\n";
    print $Sdout ".de PF\n,br\n.ne 2\n.ft 3\n.nf\n..\n.de FP\n.br\n\.ne 2\n\.ft 1\n.fi\n..\n";
    print $Sdout ".BG\n";
    print $Sdout ".FN ", $blocks{"name"}, "\n";
    print $Sdout ".TL\n";
    print $Sdout $blocks{"title"}, "\n";
    if (defined $blocks{"description"}){
	print $Sdout ".DN\n", text2nroff($blocks{"description"}), "\n";
    }
    if (defined $blocks{"usage"}){
	print $Sdout ".CS\n", code2nroff($blocks{"usage"}), "\n";
    }
    Sd_print_argblock("arguments", ".RA");
    Sd_print_argblock("value", ".RT");
    Sd_print_block("details", ".DT");
    Sd_print_sections();
    Sd_print_block("note", "Note");
    Sd_print_block("references", ".SH REFERENCES");
    print $Sdout "\n";
    Sd_print_block("seealso", ".SA");
    print $Sdout "\n";
    Sd_print_codeblock("examples", ".EX");
    while ($#keywords >= 0) {
	print $Sdout ".KW ", shift( @keywords ), "\n";
    }
    print $Sdout ".WR\n";
}

## Print a standard block
sub Sd_print_block {

    my ($block,$macro) = @_;

    if(defined $blocks{$block}){
	print $Sdout $macro, "\n", text2nroff($blocks{$block});
    }
}

## Print a code block (preformatted)
sub Sd_print_codeblock {

    my ($block, $macro) = @_;
    my $ntext;

    if(defined $blocks{$block}){
	$ntext = code2txt($blocks{$block});
	# make sure there is precisely one leading "\n"
	$ntext =~ s/^[\n]*//go;
	$ntext = "\n". $ntext;
	$ntext =~ s/\\&\././go;
	$ntext =~ s/\\\\/\\/go;
	print $Sdout $macro, $ntext;
    }
}

## Print the value or arguments block
sub Sd_print_argblock {

    my ($block, $macro) = @_;

    if(defined $blocks{$block}){
	print $Sdout $macro, "\n" if $macro;
	my $text = $blocks{$block};

	if($text =~ /\\item/s){
	    $text =~ /^(.*)(\\item.*)*/s;
	    my ($begin, $rest) = split(/\\item/, $text, 2);
	    if($begin){
		print $Sdout &text2nroff($begin);
		$text =~ s/^$begin//s;
	    }
	    my $loopcount = 0;
	    while(checkloop($loopcount++, $text, "\\item") &&
		  $text =~ /\\item/s){
		my ($id, $arg, $desc)  = get_arguments("item", $text, 2);
		$arg = text2nroff($arg);
		$desc = text2nroff($desc);
		print $Sdout ".AG ", $arg, "\n";
		print $Sdout $desc, "\n";
		$text =~ s/.*$id//s;
	    }
	}
	else{
	    print $Sdout &text2nroff($text), "\n";
	}
    }
}

## Print sections
sub Sd_print_sections {

    my $section;

    for($section=0; $section<$max_section; $section++){
	print $Sdout "\n";
	print $Sdout ".SH ";
	print $Sdout $section_title[$section], "\n";
	print $Sdout &text2nroff($section_body[$section]), "\n";
    }
}

#==**nroff support****

## Convert a Rdoc text string to nroff
##   $_[0]: text to be converted
##   $_[1]: (optional) indentation of paragraphs. default = $INDENT

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
    $text =~ s/\\le/<=/go;
    $text =~ s/\\ge/>=/go;
    $text =~ s/\\R/R/go;
    foreach my $cmd (@special_commands) {
	$text = transform_command($text, $cmd, $ECMD . $cmd,
				  "-", "$EDASH");
    }
    $text =~ s/---/--/go;
    $text =~ s/--/-/go;
    foreach my $cmd (@special_commands) {
	$text = transform_command($text, $ECMD . $cmd, $cmd,
				  "$EDASH", "-");
    }
    $text =~ s/$EOB/\{/go;
    $text =~ s/$ECB/\}/go;

    $text = undefine_command($text, "link");
    $text = undefine_command($text, "textbf");
    $text = undefine_command($text, "mathbf");
    $text = undefine_command($text, "email");

    $text = replace_command($text, "file", "'", "'");
    $text = replace_command($text, "url", "<URL: ", ">");

    $text = replace_command($text, "emph", "_", "_");
    $text = replace_command($text, "bold", "*", "*");
    $text = replace_command($text, "strong", "*", "*");

    $text = undefine_command($text, "acronym");
    $text = undefine_command($text, "cite");
    $text = undefine_command($text, "dfn");

    $text = replace_command($text, "command", "'", "'");
    $text = replace_command($text, "env", "'", "'");
    $text = replace_command($text, "kbd", "'", "'");
    $text = replace_command($text, "option", "'", "'");
    $text = replace_command($text, "pkg", "'", "'");
    $text = replace_command($text, "samp", "'", "'");

    $text = undefine_command($text, "var");

    $text = replace_command($text, "sQuote", "'", "'");
    $text = replace_command($text, "dQuote", "\"", "\"");

    ## Handle equations:
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

    ## Handle '\describe':
    $text = replace_command($text,
			    "describe",
			    "\n.in +$INDENT\n",
			    "\n.in -$INDENT\n");
    while(checkloop($loopcount++, $text, "\\item")
	  && $text =~ /\\itemnormal/s) {
	my ($id, $arg, $desc)  = get_arguments("item", $text, 2);
	$arg = text2nroff($arg);
	$descitem = ".IP \"\" $TAGOFF\n".
	    ".ti -\\w\@" . $arg .
	    "\\ \@u\n" . $arg . "\\ " . text2nroff($desc);
	$descitem =~ s/\\&\././go;
	$text =~ s/\\itemnormal.*$id/$descitem/s;
    }
    $text = nroff_unescape_codes($text);
    unmark_brackets($text);
}

sub nroff_unescape_codes {

    my $text = $_[0];

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "escaped code")
	  && $text =~ /$ECODE($ID)/) {
	my $id = $1;
	my $ec = code2nroff($ecodes{$id});
	$text =~ s/$ECODE$id/\'$ec\'/;
    }

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "escaped preformat")
	  && $text =~ /$EPREFORMAT($ID)/){
	my $id = $1;
	my $ec = code2nroff($epreformats{$id});
	$text =~ s/$EPREFORMAT$id/.PF\n$ec.FP/;
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
    $text = replace_addnl_command($text, "dontrun",
				  "## Not run: ", "## End(Not run)");
    $text = drop_full_command($text, "testonly");
    $text = drop_full_command($text, "dontshow");

    $text = unmark_brackets($text);

    $text = transform_S3method($text);
    $text = transform_S4method($text);

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

	## remove trailing \cr (otherwise we get an empty last line)
	$arg =~ s/\\cr\s*$//go;

	## parse the format of the tabular environment
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

	## now do the real work: split into lines and columns
	my @rows = split(/\\cr/, $arg);
	for($k=0; $k<=$#rows;$k++){
	    my @cols = split(/\\tab/, $rows[$k]);
	    die("Error:\n  $rows[$k]\\cr\n" .
		"does not fit tabular format \{$format\}\n")
		if ($#cols != $#colformat);
	    for($l=0; $l<$#cols; $l++){
		$cols[$l] =~ s/^\s*(.*)\s*$/$1/;
		$table .= "$cols[$l]\t";
	    }
	    $cols[$#cols] =~ s/^\s*(.*)\s*$/$1/;
	    $table .= "$cols[$#cols]\n";
	}
	$table .= ".TE\n";

	$text =~ s/\\tabular.*$id/$table/s;
    }

    $text;
}

#==********************* Example ***********************************

sub rdoc2ex { # (filename)

    my $tit = striptitle($blocks{"title"});

    if(defined $blocks{"examples"}) {
	local $Exout;
	if($_[0]) {
	    $Exout = new FileHandle;
	    open $Exout, "> $_[0]"; # will be closed when goes out of scope
	} else {
	    $Exout = "STDOUT";
	}

	$tit =~ s/\s+/ /g;

	$Exout->print(wrap("### Name: ", "###   ", $blocks{"name"}),
		      "\n",
		      wrap("### Title: ", "###   ", $tit),
		      "\n",
		      wrap("### Aliases: ", "###   ", @aliases),
		      "\n",
		      wrap("### Keywords: ", "###   ", @keywords),
		      "\n\n");

	ex_print_exampleblock("examples", "Examples");

	$Exout->print("\n\n");
    }
}

sub ex_print_exampleblock {

    my ($block,$env) = @_;

    if(defined $blocks{$block}) {
	$Exout->print("### ** Examples\n",
		      code2examp($blocks{$block}),
		      "\n");
    }
}

sub code2examp {
    ##-	similar to ..2latex
    my $text = $_[0];

    $text =~ s/\\%/%/go;
    $text =~ s/\\ldots/.../go;
    $text =~ s/\\dots/.../go;

    $text = undefine_command($text, "link");

    $text = replace_prepend_command($text, "dontshow",
				    "## Don't show: ", "## End Don't show", "");
    $text = replace_prepend_command($text, "testonly",
				    "## Don't show:", "## End Don't show", "");

    $text = replace_prepend_command($text, "dontrun",
				    "## Not run: ", "## End(Not run)",
				    "##D ");
    $text =~ s/\\\\/\\/g;

    $text = unmark_brackets($text);

    $text = transform_S3method($text);
    $text = transform_S4method($text);

    $text;
}


#==********************* LaTeX ***********************************

sub ltxstriptitle { # text
    my $text = $_[0];
    $text =~ s/\\R/\\R\{\}/go;
    return $text;
}
sub foldorder {uc($a) cmp uc($b) or $a cmp $b;}

sub rdoc2latex {# (filename)

    my $c, $a, $blname;

    local $latexout;
    if($_[0]) {
	$latexout = new FileHandle;
	open $latexout, "> $_[0]";  # will be closed when goes out of scope
    } else {
	$latexout = "STDOUT";
    }
    $blname = &latex_escape_name($blocks{"name"});
    print $latexout "\\HeaderA\{";
    print $latexout $blname;
    print $latexout "\}\{";
    print $latexout &ltxstriptitle($blocks{"title"});
    print $latexout "\}\{";
    print $latexout &latex_link_trans0($blocks{"name"});
    print $latexout "\}\n";

    my $current = $blocks{"name"}, $generic, $cmd;
    foreach (sort foldorder @aliases) {
	next if (/\(/ || /\{/ || /\{-class/); # these two break the PDF
                                              # indexing
	$generic = $a = $_;
	$generic =~ s/\.data\.frame$/.dataframe/o;
	$generic =~ s/\.model\.matrix$/.modelmatrix/o;
	$generic =~ s/\.[^.]+$//o;
	if ($generic ne "" && $generic eq $current && $generic ne "ar") {
	    $cmd = "methaliasA"
	} else { $cmd = "aliasA"; $current = $a; }

	$c = code2latex($_,0);
	$a = latex_code_alias($c);
	print STDERR "rdoc2l: alias='$_', code2l(.)='$c', latex_c_a(.)='$a'\n"
	    if $debug;
	printf $latexout "\\%s\{%s\}\{%s\}\{%s\}\n", $cmd, $a, 
	       $blname, latex_link_trans0($a)
	unless /^\Q$blocks{"name"}\E$/; # Q..E : Quote (escape) Metacharacters
    }
    foreach (@keywords) {
	printf $latexout "\\keyword\{%s\}\{%s\}\n", $_, $blname unless /^$/ ;
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

    print $latexout "\n";
}

## The basic translator for 'normal text'
sub text2latex {

    my $text = $_[0];

    $text =~ s/$EOB/\\\{/go;
    $text =~ s/$ECB/\\\}/go;

    $text =~ s/\\cite/\\Cite/go;

    $text =~ s/\\itemize/\\Itemize/go;
    $text =~ s/\\enumerate/\\Enumerate/go;
    $text =~ s/\\tabular/\\Tabular/go;
    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\eqn")
	  &&  $text =~ /\\eqn/){
	my ($id, $eqn, $ascii) = get_arguments("eqn", $text, 2);
	## $ascii may be empty
	$text =~ s/\\eqn.*$id/\\eeeeqn\{$eqn\}\{$ascii\}/s;
    }

    $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\deqn")
	  && $text =~ /\\deqn/) {
	my ($id, $eqn, $ascii) = get_arguments("deqn", $text, 2);
	$text =~ s/\\deqn.*$id/\\dddeqn\{$eqn\}\{$ascii\}/s;
    }

    $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\item")
	  && $text =~ /\\itemnormal/s) {
	my ($id, $arg, $desc) = get_arguments("item", $text, 2);
	$descitem = "\\DITEM[" . text2latex($arg) . "] " . text2latex($desc);
	$text =~ s/\\itemnormal.*$id/$descitem/s;
    }

    $text =~ s/\\eeeeqn/\\eqn/go;
    $text =~ s/\\dddeqn/\\deqn/og;
    $text =~ s/\\DITEM/\\item/og;

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "escaped preformat")
	  && $text =~ /$EPREFORMAT($ID)/){
	my $id = $1;
	my $ec = latex_preformat_cmd(code2latex($epreformats{$id},1));
	$text =~ s/$EPREFORMAT$id/$ec/;
    }

    $text =~ s/\\\\/\\bsl{}/go;
    ## A mess:  map  & \& \\& \\\& to  \& \& \bsl{}\& \bsl{}\&
    $text =~ s/([^\\])&/$1\\&/go;
    $text =~ s/\\R(\s+)/\\R\{\}$1/go;
    $text =~ s/\\cr\n\[/\\\\\{\}\n\[/go;
    $text =~ s/\\cr/\\\\/go;
    $text =~ s/\\tab(\s+)/&$1/go;

    ## we need to convert \links's
    while(checkloop($loopcount++, $text, "\\link")
	  &&  $text =~ /\\link/){
	my ($id, $arg, $opt) = get_link($text);
	my $mapped_name = &latex_link_trans0($arg);
	$text =~ s/\\link(\[.*\])?$id.*$id/\\LinkA{$arg}{$mapped_name}/s;
    }


    ##-- We should escape $LATEX_SPEC  unless within 'eqn' above ...
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

    ##    $text =~ s/\\\\/\\bsl{}/go;
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
    $text = replace_addnl_command($text, "dontrun",
				  "## Not run: ", "## End(Not run)");
    $text = drop_full_command($text, "testonly");
    $text = drop_full_command($text, "dontshow");

    $text = unmark_brackets($text);

    $text = transform_S3method($text);
    $text = transform_S4method($text);

    $text;
}

sub latex_preformat_cmd {

    my $code = $_[0];

    $code = latex_code_trans ($code);
    $code = "\\begin\{alltt\}" . $code . "\\end\{alltt\}";
    $code;
}

sub latex_print_block {

    my ($block,$env) = @_;

    if(defined $blocks{$block}){
	print $latexout "\\begin\{$env\}\\relax\n";
	my $thisblock = &text2latex($blocks{$block});
	print $latexout $thisblock;
	print $latexout "\n" unless
	    $thisblock =~ /\n$/ || length($thisblock) == 0;
	print $latexout "\\end\{$env\}\n";
    }
}

sub latex_print_codeblock {

    my ($block,$env) = @_;

    if(defined $blocks{$block}){
	print $latexout "\\begin\{$env\}\n";
	print $latexout "\\begin\{verbatim\}";
	print $latexout &code2latex($blocks{$block},0);
	print $latexout "\\end\{verbatim\}\n";
	print $latexout "\\end\{$env\}\n";
    }
}

sub latex_print_exampleblock {

    my ($block,$env) = @_;

    if(defined $blocks{$block}){
	print $latexout "\\begin\{$env\}\n";
	print $latexout "\\begin\{ExampleCode\}";
	my $out = code2latex($blocks{$block},0);
	$out =~ s/\\\\/\\/go;
	print $latexout $out;
	print $latexout "\\end\{ExampleCode\}\n";
	print $latexout "\\end\{$env\}\n";
    }
}

sub latex_print_argblock {

    my ($block,$env) = @_;

    if(defined $blocks{$block}){

	print $latexout "\\begin\{$env\}\n";

	my $text = $blocks{$block};

	if($text =~ /\\item/s){#-- if there is >= 1 "\item":  ldescription
	    $text =~ /^(.*)(\\item.*)*/s;
	    my ($begin, $rest) = split(/\\item/, $text, 2);
	    if($begin){
		print $latexout &text2latex($begin);
		$text =~ s/^$begin//s;
	    }
	    print $latexout "\\begin\{ldescription\}\n";
	    my $loopcount = 0;
	    while(checkloop($loopcount++, $text, "\\item")
		  &&  $text =~ /\\item/s){
		my ($id, $arg, $desc)  = get_arguments("item", $text, 2);
		print $latexout "\\item\[";
		print $latexout &latex_code_cmd(code2latex($arg,1));
		print $latexout "\] ";
		print $latexout &text2latex($desc), "\n";
		$text =~ s/.*$id//s;
	    }
	    print $latexout "\\end\{ldescription\}\n";
	    my $thisblock = &text2latex($text);
	    print $latexout $thisblock;
	    print $latexout "\n" unless 
		$thisblock =~ /\n$/ || length($thisblock) == 0;
	}
	else{
	    my $thisblock = &text2latex($text);
	    print $latexout $thisblock;
	    print $latexout "\n" unless 
		$thisblock =~ /\n$/ || length($thisblock) == 0;
	}
	print $latexout "\\end\{$env\}\n";
    }
}

sub latex_print_sections {

    my $section;

    for($section=0; $section<$max_section; $section++){
	print $latexout "\\begin\{Section\}\{" . $section_title[$section] . "\}\n";
	my $thisblock = &text2latex($section_body[$section]);
	print $latexout $thisblock;
	print $latexout "\n" unless
	    $thisblock =~ /\n$/ || length($thisblock) == 0;
	print $latexout "\\end\{Section\}\n";
    }
}

sub latex_unescape_codes {

    my $text = $_[0];

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "escaped code")
	  && $text =~ /$ECODE($ID)/) {
	my $id = $1;
	my $ec = latex_code_cmd(code2latex($ecodes{$id},1));
	$text =~ s/$ECODE$id/$ec/;
    }

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "escaped preformat")
	  && $text =~ /$EPREFORMAT($ID)/){
	my $id = $1;
	my $ec = latex_preformat_cmd(code2latex($epreformats{$id},1));
	$text =~ s/$EPREFORMAT$id/$ec/;
    }

    $text;
}

sub latex_escape_name {
    my $c = $_[0];

    $c = unmark_brackets($c);
    if($c =~ /[$LATEX_SPECIAL]/){
	$c =~ s/[$LATEX_SPECIAL]/\\$&/go; #- escape them
    }
    $c =~ s/\\\^/\\textasciicircum{}/go;# ^ is SPECIAL
    $c =~ s/\\~/\\textasciitilde{}/go;
    $c =~ s/\\\\\\%/\\Rpercent{}/go;
    $c =~ s/\\\{/\\textbraceleft{}/go;
    $c =~ s/\\\}/\\textbraceright{}/go;
    $c =~ s/\\\\\\\\/\\textbackslash{}/go;
    ## avoid conversion to guillemots
    $c =~ s/<</<\{\}</;
    $c =~ s/>>/>\{\}>/;
    $c;
}

## The next two should transform links and aliases identically so use
## common subroutines

sub latex_code_trans {
    my $c = $_[0];
    my $BSL = '@BSL@';

    if($c =~ /[$LATEX_SPECIAL]/){
	$c =~ s/\\\\/$BSL/go;
	$c =~ s/\\([$LATEX_SPECIAL])/$1/go; #- unescape them (should not be escaped)
	$c =~ s/[$LATEX_SPECIAL]/\\$&/go; #- escape them
#	$c =~ s/\\\^/\$\\,\\hat{\\,}\$/go;# ^ is SPECIAL
#	$c =~ s/\\~/\$\\,\\tilde{\\,}\$/go;
	$c =~ s/\\\^/\\textasciicircum{}/go;# ^ is SPECIAL
	$c =~ s/\\~/\\textasciitilde{}/go;
	$c =~ s/$BSL/\\bsl{}/go;
    }
    ## avoid conversion to guillemots
    $c =~ s/<</<\{\}</;
    $c =~ s/>>/>\{\}>/;
    $c =~ /HYPERLINK\(([^)]*)\)/;
    my $c0 = $1;
    my $link = latex_link_trans($c0);
    $c0 = latex_link_trans0($c0);
    $c =~ s/HYPERLINK\([^)]*\)/\\LinkA{$link}{$c0}/go;
    $c =~ s/,,/,{},/g; # ,, is a ligature in the ae font.
    $c;
}

sub latex_link_trans {
    my $c = $_[0];
    $c =~ s/<-\./<\\Rdash\./go;
    $c =~ s/<-$/<\\Rdash/go;
    $c;
}

sub latex_code_cmd {

    my $code = $_[0];

    $code = latex_code_trans ($code);
    $code = "\\code\{" . $code . "\}";
    $code;
}

sub latex_link_trans0 {
    my $c = $_[0];

    $c = unmark_brackets($c);
    $c =~ s/\\Rdash/.Rdash./go;
    $c =~ s/-/.Rdash./go;
    $c =~ s/\\_/.Rul./go;
    $c =~ s/\\\$/.Rdol./go;
    $c =~ s/\\\^/.Rcaret./go;
    $c =~ s/\^/.Rcaret./go;
    $c =~ s/_/.Rul./go;
    $c =~ s/\$/.Rdol./go;
    $c =~ s/\\#/.Rhash./go;
    $c =~ s/#/.Rhash./go;
    $c =~ s/\\&/.Ramp./go;
    $c =~ s/&/.Ramp./go;
    $c =~ s/\\~/.Rtilde./go;
    $c =~ s/~/.Rtilde./go;
    $c =~ s/\\%/.Rpcent./go;
    $c =~ s/%/.Rpcent./go;
    $c =~ s/\\\\/.Rbl./go;
    $c =~ s/\{/.Rlbrace./go;
    $c =~ s/\}/.Rrbrace./go;
    $c;
}


## Tough examples are
##	Logic.Rd  Arithmetic.Rd	 Extract.Rd  formula.Rd
sub latex_code_alias {

    my $c = $_[0];  ##-- $c is (typically) the OUTPUT of  code2latex(.) :
    $c = latex_code_trans ($c);
    $c = latex_link_trans ($c);
    $c =~ s/\!/"!/go; # "  This is the bibtex escape
    $c =~ s/\|/"|/go; # "
    ##      $c =~ s/@/"@/go; # "  Not currently valid R character
    $c;
}

#==************************ Compiled HTML ********************************

sub rdoc2chm { # (filename) ; 0 for STDOUT

    local $htmlout;
    if($_[0]) {
	$htmlout = new FileHandle;
	open $htmlout, "> $_[0]";  # will be closed when goes out of scope
    } else {
	$htmlout = "STDOUT";
    }
    $using_chm = 1;
    $nlink = 0;
    print $htmlout (chm_functionhead(striptitle($blocks{"title"}), $pkgname,
				     &html_escape_name($blocks{"name"})));

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

    JScript() if $using_chm && $nlink > 0;
    print $htmlout (html_functionfoot($pkgname, $version));
    if($_[0]) { close $htmlout; }
    $using_chm = 0;
}

sub mklink {
    $nlink++;
    "onclick=\"findlink('" . $_[0] . "', '" . $_[1] . "')\" " .
       "style=\"text-decoration: underline; color: blue; cursor: hand\""
}

sub JScript {
    print $htmlout <<END
<script Language="JScript">
function findlink(pkg, fn) {
var Y, link;
Y = location.href.lastIndexOf("\\\\") + 1;
link = location.href.substring(0, Y);
link = link + "../../" + pkg + "/chtml/" + pkg + ".chm::/" + fn;
location.href = link;
}
</script>
END
}

#==************************ S Sgml ********************************

sub rdoc2Ssgm { # (filename) ; 0 for STDOUT

    local $sgmlout;
    if($_[0]) {
	$sgmlout = new FileHandle;
	open $sgmlout, "> $_[0]";  # will be closed when goes out of scope
    } else {
	$sgmlout = "STDOUT";
    }
    print $sgmlout (Ssgm_functionhead($blocks{"name"}, $blocks{"title"}));

    Ssgm_print_block("description", "s-description");
    Ssgm_print_usage();
    Ssgm_print_argblock();
    Ssgm_print_block_named("format", "Format");
    Ssgm_print_block("details", "s-details");
    Ssgm_print_valueblock();

    Ssgm_print_sections();

    ## s-note, s-author, s-references are in the DTD, but not translated
    ## to HTML.

    ##    Ssgm_print_block("note", "s-note");
    Ssgm_print_block_named("note", "Note");
    ##    Ssgm_print_block("author", "s-author");
    Ssgm_print_block_named("author", "Author(s)");
    Ssgm_print_block_named("source", "Source");
    ##    Ssgm_print_block("references", "s-references");
    Ssgm_print_block_named("references", "References");
    Ssgm_print_seealso();
    Ssgm_print_examples();
    if ($#keywords > 0) {
	print $sgmlout "<s-keywords>\n";
	while ($#keywords >= 0) {
	    print $sgmlout "<s-keyword>", shift( @keywords ),
	    "</s-keyword>\n";
	}
	print $sgmlout "</s-keywords>\n";
    }

    print $sgmlout (Ssgm_functionfoot());
}

## Convert a Rdoc text string to HTML, i.e., convert \code to <tt> etc.
sub text2Ssgm {

    my $text = $_[0];
    my $outerpass = $_[1];
    my $inarglist = $_[2];

    if($outerpass) {
        $text =~ s/&([^#])/&amp;\1/go; # might have explicit &# in source
	$text =~ s/>/&gt;/go;
	$text =~ s/</&lt;/go;
	$text =~ s/\]/&rsqb;/go;
	$text =~ s/\[/&lsqb;/go;
	$text =~ s/\\%/%/go;

	$text =~ s/\n\s*\n/\n<p>\n/sgo;
	$text =~ s/\\dots/.../go;
	$text =~ s/\\ldots/.../go;

	$text =~ s/\\mu/&mu;/go;
	$text =~ s/\\Gamma/&Gamma;/go;
	$text =~ s/\\alpha/&alpha;/go;
	$text =~ s/\\Alpha/&Alpha;/go;
	$text =~ s/\\pi/&pi;/go;
	$text =~ s/\\sigma/&sigma;/go;
	$text =~ s/\\Sigma/&Sigma;/go;
	$text =~ s/\\lambda/&lambda;/go;
	$text =~ s/\\beta/&beta;/go;
	$text =~ s/\\epsilon/&epsilon;/go;
	$text =~ s/\\left\(/\(/go;
	$text =~ s/\\right\)/\)/go;
	$text =~ s/\\le/&lt;=/go;# \le *after* \left !
	$text =~ s/\\ge/&gt;=/go;
	$text =~ s/\\R/<bf>R<\/bf>/go;
	foreach my $cmd (@special_commands) {
	    $text = transform_command($text, $cmd, $ECMD . $cmd,
				      "-", "$EDASH");
	}
	$text =~ s/---/&mdash;/go;
	$text =~ s/--/&ndash;/go;
	foreach my $cmd (@special_commands) {
	    $text = transform_command($text, $ECMD . $cmd, $cmd,
				      "$EDASH", "-");
	}
	$text =~ s/$EOB/\{/go;
	$text =~ s/$ECB/\}/go;
    }

    $text = replace_command($text, "emph", "<em>", "</em>");
    $text = replace_command($text, "bold", "<bf>", "</bf>");
    $text = replace_command($text, "strong", "<bf>", "</bf>");

    $text = replace_command($text, "file", "'<tt>", "</tt>'");

    $text = undefine_command($text, "acronym");
    $text = undefine_command($text, "cite");
    $text = undefine_command($text, "dfn");

    $text = replace_command($text, "command", "'<tt>", "</tt>'");
    $text = replace_command($text, "env", "'<tt>", "</tt>'");
    $text = replace_command($text, "kbd", "'<tt>", "</tt>'");
    $text = replace_command($text, "option", "'<tt>", "</tt>'");
    $text = replace_command($text, "pkg", "'<tt>", "</tt>'");
    $text = replace_command($text, "samp", "'<tt>", "</tt>'");

    $text = undefine_command($text, "var");

    $text = replace_command($text, "sQuote", "'", "'");
    $text = replace_command($text, "dQuote", "\"", "\"");


    $text = Ssgm_tables($text);
    $text =~ s/\\cr/<br>/sgo;

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\link")
	  &&  $text =~ /\\link/){
	my ($id, $arg, $opt) = get_link($text);
	$text =~
	    s/\\link(\[.*\])?$id.*$id/<s-function name="$arg">$arg<\/s-function>/s;
    }

    $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\email")
	  &&  $text =~ /\\email/){
	my ($id, $arg)	= get_arguments("email", $text, 1);
	$text =~ s/\\email$id.*$id/<url url=\"mailto:$arg\">/s;
    }

    $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\url")
	  &&  $text =~ /\\url/){
	my ($id, $arg)	= get_arguments("url", $text, 1);
	$text =~ s/\\url.*$id/<url url =\"$arg\">/s;
    }

    ## Handle equations:
    $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\eqn")
	  &&  $text =~ /\\eqn/){
	my ($id, $eqn, $ascii) = get_arguments("eqn", $text, 2);
	$eqn = $ascii if $ascii;
	$text =~ s/\\eqn(.*)$id/<it>$eqn<\/it>/s;
    }

    $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\deqn")
	  &&  $text =~ /\\deqn/){
	my ($id, $eqn, $ascii) = get_arguments("deqn", $text, 2);
	$eqn = $ascii if $ascii;
	$text =~ s/\\deqn(.*)$id/<p><it>$eqn<\/it><\/p>/s;
    }

    $text = replace_command($text, "itemize", "<itemize>", "</itemize>");
    $text = replace_command($text, "enumerate", "<enum>", "</enum>");
    $text =~ s/<\/p>\n<p>\s+\\item\s+/<item>/go;
    $text =~ s/\\item\s+/<item>/go;

    ## Handle '\describe':
    $text = replace_command($text, "describe", "<descrip>", "</descrip>\n");
    while(checkloop($loopcount++, $text, "\\item")
	  && $text =~ /\\itemnormal/s) {
	my ($id, $arg, $desc)  = get_arguments("item", $text, 2);
	$descitem = "<tag/" . text2Ssgm($arg, 0, $inarglist) . "/";
	$descitem .= text2Ssgm($desc, 0, $inarglist);
	$text =~ s/\\itemnormal.*$id/$descitem/s;
    }
    if($outerpass) {
	$text =~ s/\\([^\\])/$1/go;#-drop single "\" (as in '\R')
	$text =~ s/\\\\/\\/go;
	$text = Ssgm_unescape_codes($text);
	$text = unmark_brackets($text);
	$text =~ s/<tag\/<s-expression>(.*?)<\/s-expression>/<tag\/$1/g;
    }
    $text;
}

sub code2Ssgm {

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
	$text =~
	    s/\\link(\[.*\])?$id.*$id/<s-function name="$arg">$arg<\/s-function>/s;
    }

    $text = replace_addnl_command($text, "dontrun",
				  "## Not run: ", "## End(Not run)");
    $text = drop_full_command($text, "testonly");
    $text = drop_full_command($text, "dontshow");
    $text =~ s/\\\\/\\/go;

    $text = unmark_brackets($text);

    $text = transform_S3method($text);
    $text = transform_S4method($text);

    $text;
}

sub see2Ssgm {

    my $text = $_[0];
    my $loopcount = 0;
    $text = Ssgm_unescape_codes($text);
    while(checkloop($loopcount++, $text, "\\link")
	  &&  $text =~ /\\link/){
	my ($id, $arg, $opt) = get_link($text);
	$text =~
	    s/\\link(\[.*\])?$id.*$id/<s-function name="$arg">$arg<\/s-function>/s;
    }

    $text = unmark_brackets($text);

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "escaped preformat")
	  && $text =~ /$EPREFORMAT($ID)/){
	my $id = $1;
	my $ec = code2Ssgm($epreformats{$id});
	if($ec =~ /<s-function/) {
	    # <s-expression cannot contain <s-function>
	    $text =~ s/$EPREFORMAT$id/$ec/;
	} else {
	    $text =~ s/$EPREFORMAT$id/<s-expression>$ec<\/s-expression>/;
	}
    }

    $text;
}

## Print a standard block
sub Ssgm_print_block {

    my ($block,$sname) = @_;

    Ssgm_print_a_section("<$sname>", $blocks{$block}, "</$sname>")
	if defined $blocks{$block};
}

sub Ssgm_print_block_named {

    my ($block,$name) = @_;

    Ssgm_print_a_section("<s-section name=\"".uc($name)."\">",
			 $blocks{$block}, "</s-section>")
	if defined $blocks{$block};
}

sub Ssgm_print_usage {

    if(defined $blocks{"usage"}){
	print $sgmlout ("<s-usage>\n<s-old-style-usage>",
			code2Ssgm($blocks{"usage"}),
			"</s-old-style-usage>\n</s-usage>\n\n");
    }
}

sub Ssgm_print_examples {

    if(defined $blocks{"examples"}){
	print $sgmlout ("<s-examples>\n<s-example type = text>",
			code2Ssgm($blocks{"examples"}),
			"</s-example>\n</s-examples>\n");
    }
}

sub Ssgm_print_seealso {

    if(defined $blocks{"seealso"}){
	print $sgmlout ("<s-see>\n", see2Ssgm($blocks{"seealso"}),
			"\n</s-see>\n\n");
    }
}


## Print the value or arguments block
sub Ssgm_print_argblock {

    my $block = "arguments";

    if(defined $blocks{$block}){
	print $sgmlout "<s-args>\n";

	my $text = $blocks{$block};

	if($text =~ /\\item/s){
	    $text =~ /^(.*)(\\item.*)*/s;
	    my ($begin, $rest) = split(/\\item/, $text, 2);
	    if($begin){
		$text =~ s/^$begin//s;
		$begin =~ s/(\n)+$//;
		print $sgmlout (text2Ssgm($begin, 1, 1), "\n");
	    }
	    my $loopcount = 0;
	    while(checkloop($loopcount++, $text, "\\item")
		  && $text =~ /\\item/s) {
		my ($id, $arg, $desc)  =
		    get_arguments("item", $text, 2);
		print $sgmlout ("<s-arg name=\"",
				text2Ssgm($arg, 1, 1),
				"\">\n",
				text2Ssgm($desc, 1, 1), "</s-arg>\n");
		$text =~ s/.*$id//s;
	    }
	    my $rest = text2Ssgm($text, 1, 1);
	    print $sgmlout ($rest, "\n") if $rest;
	}
	else{
	    my $rest = text2Ssgm($text, 1, 1);
	    print $sgmlout ($rest, "\n") if $rest;
	}
	print $sgmlout "</s-args>\n\n";
    }
}

sub Ssgm_print_valueblock {

    my $block = "value";

    if(defined $blocks{$block}){
	print $sgmlout "<s-value>\n";

	my $text = $blocks{$block};

	if($text =~ /\\item/s){
	    $text =~ /^(.*)(\\item.*)*/s;
	    my ($begin, $rest) = split(/\\item/, $text, 2);
	    if($begin){
		$text =~ s/^$begin//s;
		$begin =~ s/(\n)+$//;
		print $sgmlout (text2Ssgm($begin, 1, 1), "\n");
	    }
	    my $loopcount = 0;
	    while(checkloop($loopcount++, $text, "\\item")
		  && $text =~ /\\item/s) {
		my ($id, $arg, $desc)  =
		    get_arguments("item", $text, 2);
		print $sgmlout ("<s-return-component name=\"",
				text2Ssgm($arg, 1, 1),
				"\">\n",
				text2Ssgm($desc, 1, 1),
				"</s-return-component>\n");
		$text =~ s/.*$id//s;
	    }
	    my $rest = text2Ssgm($text, 1, 1);
	    print $sgmlout ($rest, "\n") if $rest;
	}
	else{
	    my $rest = text2Ssgm($text, 1, 1);
	    print $sgmlout ($rest, "\n") if $rest;
	}
	print $sgmlout "</s-value>\n\n";
    }
}

## Print sections
sub Ssgm_print_sections {

    my $section;

    for($section=0; $section<$max_section; $section++){
	Ssgm_print_block_named($section, $section_title[$section]);
    }
}

sub Ssgm_print_a_section {
    my ($sbegin, $body, $send) = @_;
    my $htmlbody = text2Ssgm($body, 1, 0);

    $htmlbody =~ s/<p>\s*<p/<p/g;  # before deqn
    $htmlbody =~ s/<\/p>\s*<\/p>/<\/p>/g;
    ## Attempt to close paragraphs tags, and remove spurious closings.
    $htmlbody =~ s/<\/(table|dl|ul|ol|dd)>\n+<\/p>\n/<\/\1>\n\n/g;
    $htmlbody =~ s/<\/(table|dl|ul|ol)>\n+(\w|<em|<s-expression|<b)/<\/\1>\n<p>\n\2/g;
    $htmlbody =~ s/<p>\s*<(table|dl|ul|ol|dt)/\n<\1/g;

    print $sgmlout ("$sbegin\n", $htmlbody, "\n$send\n\n");
}

sub Ssgm_unescape_codes {

    my $text = $_[0];

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "escaped code")
	  && $text =~ /$ECODE($ID)/) {
	my $id = $1;
	my $ec = code2Ssgm($ecodes{$id});
	if($ec =~ /<s-function/) {
	    # <s-expression cannot contain <s-function>
	    $text =~ s/$ECODE$id/$ec/;
	} else {
	    $text =~ s/$ECODE$id/<s-expression>$ec<\/s-expression>/;
	}
    }

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "escaped preformat")
	  && $text =~ /$EPREFORMAT($ID)/){
	my $id = $1;
	my $ec = code2Ssgm($epreformats{$id});
	if($ec =~ /<s-function/) {
	    # <s-expression cannot contain <s-function>
	    $text =~ s/$EPREFORMAT$id/$ec/;
	} else {
	    $text =~ s/$EPREFORMAT$id/<s-expression>$ec<\/s-expression>/;
	}
    }

    $text;
}

## No support for tables in DTD, even though <tabular> is in
## linuxdoc.dtd.
sub Ssgm_tables {

    my $text = $_[0];

    my $loopcount = 0;
    while(checkloop($loopcount++, $text, "\\tabular")
	  &&  $text =~ /\\tabular/){

	my ($id, $format, $arg)	 =
	    get_arguments("tabular", $text, 2);

	$arg =~ s/\n/ /sgo;

	## remove trailing \cr (otherwise we get an empty last line)
	$arg =~ s/\\cr\s*$//go;

	## parse the format of the tabular environment
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

	## now do the real work: split into lines and columns
	my $table = "<p>\n<!-- no support for tables -->\n";
	my @rows = split(/\\cr/, $arg);
	for($k=0; $k<=$#rows;$k++){
	    $table .= "    ";
	    my @cols = split(/\\tab/, $rows[$k]);
	    die("Error:\n  $rows[$k]\\cr\n" .
		"does not fit tabular format \{$format\}\n")
		if ($#cols != $#colformat);
	    $table .= $cols[0];
	    for($l=1; $l<=$#cols; $l++){
		$table .= "|$cols[$l]";
	    }
	    $table .= "<br>\n";
	}
	$table .= "<!-- end of table -->\n";
	$text =~ s/\\tabular.*$id/$table/s;
    }

    $text;
}

sub Ssgm_title3
{
    my $title = $_[0];

    "\n<h3>$title</h3>\n\n";
}

## The header & footer of a function page
sub Ssgm_functionhead
{
    my ($name,$title) = @_;

    my $retval =
	"<!doctype s-function-doc system \"s-function-doc.dtd\" [\n".
	"<!entity % S-OLD \"INCLUDE\">\n]\n>\n".
	"<s-function-doc>\n";
    $retval .= "<s-topics>\n  <s-topic>".$name."</s-topic>\n";
    my $alias;
    for $alias (@aliases) {
        next if ($alias eq $name);
        $retval .= "  <s-topic>" . $alias . "</s-topic>\n";
    }
    $retval .= "</s-topics>\n\n";
    $retval .= "<s-title>\n".$title."\n</s-title>\n\n";
}

sub Ssgm_functionfoot
{

    "<s-docclass>\nfunction\n</s-docclass>\n</s-function-doc>\n";
}

# Local variables: **
# perl-indent-level: 4 **
# cperl-indent-level: 4 **
# page-delimiter: "^#==" **
# End: **
