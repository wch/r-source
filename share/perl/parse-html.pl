# Helper Perl script for html-to-rtf.pl
#
# based on hh2rtf by Steve Atkins at
#
# http://www.blighty.com/products/hh2rtf/  which says
#   `Freeware. You may not resell it or claim you wrote it. 
#    You can use it for anything, commercial or otherwise.'
#
# Modifications for R (C) 1999, 2001 B. D. Ripley

# HTML parser
# Jim Davis, July 15 1994

# This is an HTML parser not an SGML parser.  It does not parse a DTD,
# The DTD is implicit in the code, and specific to HTML.  
# The processing of the HTML can be customized by the user by
#   1) Defining routines to be called for various tags (see Begin and End arrays)
#   2) Defining routines html_content and html_whitespace

# This is not a validating parser.   It does not check the content model
# eg you can use DT outside a DL and it won't know.  It is too liberal in
# what tags are allowed to minimize what other tags.

# modified Nov '97 (steve@blighty.com) to support RTF generation
# modified 3 Aug to add a bunch of HTML 2.0 tags
# modified 3 Sept to print HTML stack to STDERR not STDOUT, to add new
#  routines html_begin_doc and html_end_doc for application specific cleanup
#  and to break parse_html into two pieces.
# modified 30 Sept 94.  parse_attributes now handles tag attributes that
#   don't have values.  thanks to  Bill Simpson-Young <bill@syd.dit.csiro.au>
#   for the code.
# modified 17 Apr 95 to support FORMS tags.
# modified 3 Dec 95 to support U, tolerate TABLE, TD, TR, TH
# modified 17 May 96 to support &# syntax for entities.  Thanks to
#  Robert Brown, dummy@c2.org

$whitespace_significant = 0;

# global variables: 
#  $line_buffer is line buffer
#  $line_count is input line number.

$line_buffer = "";
$line_count = 0;
$filename = "";

sub parse_html {
    local ($file) = @_;
    open (HTML, $file) || die "Could not open $file: $!\nStopped";
    $filename = $file;
    &parse_html_stream ();
    close (HTML);}

# Global input HTML is the handle to the stream of HTML
sub parse_html_stream {
    local ($token, $new);

    ## initialization
    @stack=();
    $line_count = 0;
    $line_buffer = "";

    ## application specific initialization
    &html_begin_doc();
  main:
    while (1) {

	$original = $line_buffer;
	# if whitespace does not matter, trim any leading space.
	if (! $whitespace_significant) {
	    $line_buffer =~ s/^\s+//;
	}

	# now dispatch on the type of token

	if ($line_buffer =~ /^(\s+)/) {
	    $token = $1;
	    $line_buffer = $';
	    &html_whitespace ($token);}

	# This will lose if there is more than one comment on the line!
	elsif ($line_buffer =~ /^(\<!--.*-->)/) {
	    $token = $1;
	    $line_buffer = $';
	    &html_comment ($token);}

	elsif ($line_buffer =~ /^(\<![^-][^\>]*\>)/) {
	    $token = $1;
	    $line_buffer = $';
	    &html_comment ($token);}

	elsif ($line_buffer =~ /^(\<\/[^\>]*\>)/) {
	    $token = $1;
	    $line_buffer = $';
	    &html_etag ($token);}

	elsif ($line_buffer =~ /^(\<[^!\/][^\>]*\>)/) {
	    $token = $1;
	    $line_buffer = $';
	    &html_tag ($token);}

	elsif ($original =~ /^([^\n<]+)/) {
	    $token = $1;
	    $line_buffer = $';
	    if ($line_buffer =~ /^\n/) {$token .= " ";}
	    $token = &substitute_entities($token);
	    &html_content ($token); }

	else {
	    # No valid token in buffer.  Maybe it's empty, or maybe there's an
	    # incomplete tag.  So get some more data.
	    $new = <HTML>;
	    if (! defined ($new)) {last main;}
	    # if we're trying to find a match for a tag, then get rid of embedded newline
	    # this is, I think, a kludge
	    if ($line_buffer =~ /^\</ && $line_buffer =~ /\n$/) {
		chop $line_buffer;
		$line_buffer .= " ";}
	    $line_buffer .= $new;
	    $line_count++;}
    }

    ## cleanup
    &html_end_doc();

    if ($#stack > -1) {
	print STDERR "Stack not empty at end of document\n";
	&print_html_stack();}
}


sub html_tag {
    local ($tag) = @_;
    local ($element) = &tag_element ($tag);
    local (%attributes) = &tag_attributes ($tag);

    # the tag might minimize (be an implicit end) for the previous tag
    local ($prev_element);
    while (&Minimizes(&stack_top_element(), $element)) {
	$prev_element = &stack_pop_element ();
	if ($debug)  {
	    print STDERR "MINIMIZING $prev_element with $element on $line_count\n";}
	&html_end ($prev_element, 0);}

    push (@stack, $tag);

    &html_begin ($element, $tag, %attributes);

    if (&Empty($element)) {
	pop(@stack);
	&html_end ($element, 0);}
}

sub html_etag {
    local ($tag) = @_;
    local ($element) = &tag_element ($tag);

    # pop stack until find matching tag.  This is probably a bad idea,
    # or at least too general.
    local ( $prev_element) = &stack_pop_element();
    until ($prev_element eq $element) {
	if ($debug) {
	    print STDERR "MINIMIZING $prev_element with /$element on $line_count \n";}
	&html_end ($prev_element, 0);

	if ($#stack == -1) {
	    print STDERR "No match found for /$element.  You will lose\n";
	    last;}
	$prev_element = &stack_pop_element();}

    &html_end ($element, 1);
}


# For each element, the names of elements which minimize it.
# This is of course totally HTML dependent and probably I have it wrong too
$Minimize{"DT"} = "DT:DD";
$Minimize{"DD"} = "DT:DD";
$Minimize{"LI"} = "LI";
$Minimize{"P"} = "P:DT:LI:H1:H2:H3:H4:H5:H6:BLOCKQUOTE:UL:OL:DL";
$Minimize{"TR"} = "TR";
$Minimize{"TD"} = "TD:TH";
$Minimize{"TH"} = "TD:TH";

# Does element E2 minimize E1?
sub Minimizes {
    local ($e1, $e2) = @_;
    local ($value) = 0;
    $elt = "";
    foreach $elt (split (/:/, $Minimize{$e1})) {
	if ($elt eq $e2) {$value = 1;}}
    $value;
}

$Empty{"BASE"} = 1;
$Empty{"BR"} = 1;
$Empty{"HR"} = 1;
$Empty{"IMG"} = 1;
$Empty{"ISINDEX"} = 1;
$Empty{"LINK"} = 1;
$Empty{"META"} = 1;
$Empty{"NEXTID"} = 1;
$Empty{"INPUT"} = 1;

# Empty tags have no content and hence no end tags
sub Empty {
    local ($element) = @_;
    $Empty{$element};
}

sub print_html_stack {
    print STDERR "\n  ==\n";
    foreach $elt (reverse @stack) {print STDERR "  $elt\n";}
    print STDERR "  ==========\n";
}

# The element on top of stack, if any.
sub stack_top_element {
    if ($#stack >= 0) {
	&tag_element ($stack[$#stack]);
    }
}

sub stack_pop_element {
    &tag_element (pop (@stack));}

# The element from the tag, normalized.
sub tag_element {
    local ($tag) = @_;
    $tag =~ /<\/?([^\s>]+)/;
    local ($element) = $1;
    $element =~ tr/a-z/A-Z/;
    $element;
}

# associative array of the attributes of a tag.
sub tag_attributes {
    local ($tag) = @_;
    $tag =~ /^<[A-Za-z0-9]+\s+(.*)>$/;
    &parse_attributes($1);
}

# string should be something like
# KEY="value" KEY2="longer value"  KEY3="tags o doom"
# output is an associative array (like a lisp property list)
# attributes names are not case sensitive, do I downcase them
# Maybe (probably) I should substitute for entities when parsing attributes.

sub parse_attributes {
    local ($string) = @_;
    local (%attributes);
    local ($name, $val);
  get: while (1) {
      if ($string =~ /^ *([A-Za-z]+)=\"([^\"]*)\"/) {
	  $name = $1;
	  $val = $2;
	  $string = $';
	  $name =~ tr/A-Z/a-z/;
	  $attributes{$name} = $val; }
      elsif ($string =~ /^ *([A-Za-z]+)=(\S*)/) {
	  $name = $1;
	  $val = $2;
	  $string = $';
	  $name =~ tr/A-Z/a-z/;
	  $attributes{$name} = $val;}
      elsif ($string =~ /^ *([A-Za-z]+)/) {
	  $name = $1;
	  $val = "";
	  $string = $';
	  $name =~ tr/A-Z/a-z/;
	  $attributes{$name} = $val;}
      else {last;}
  }
    %attributes;
}

sub substitute_entities {
    local ($string) = @_;
    $string =~ s/&amp;/&/og;
    $string =~ s/&lt;/</og;
    $string =~ s/&gt;/>/og;
    $string =~ s/&quot;/\"/og;
    $string =~ s/&nbsp;/ /og;
    $string =~ s/&rarr;/->/og;
    $string =~ s/&reg;/(tm)/og;
    $string =~ s/&#150;/{--}/og; # en dash
    $string =~ s/&#151;/{---}/og;# em dash
    local($ch);
    while ($string =~ /\&#([^;]*);/) {
	   $ch=sprintf("%c", $1);
	   $string =~ s/\&#[^;]*;/$ch/;
       }
    $string;
}


@HTML_elements = (
		  "A",
		  "ADDRESS",
		  "B",
		  "BASE",
		  "BLINK",	#  Netscape addition :-(
		  "BLOCKQUOTE",
		  "BODY",
		  "BR",
		  "CITE",
		  "CENTER",	# Netscape addition :-(
		  "CODE",
		  "DD",
		  "DIR",
		  "DIV",
		  "DFN",
		  "DL",
		  "DT",
		  "EM",
		  "FONT",       # Yach!
		  "FORM",
		  "H1", "H2", "H3", "H4", "H5", "H6",
		  "HEAD",
		  "HR",
		  "HTML",
		  "I",
		  "ISINDEX",
		  "IMG",
		  "INPUT",
		  "KBD",
		  "LI",
		  "LINK",
		  "MENU",
		  "META",
		  "NEXTID",
		  "OBJECT", 
		  "OL",
		  "OPTION",
		  "P",
		  "PARAM",
		  "PRE",
		  "SAMP",
		  "SCRIPT",
		  "SELECT",
		  "STRIKE",
		  "STRONG",
		  "TABLE",
		  "TD",
		  "TH",
		  "TR",
		  "TITLE",
		  "TEXTAREA",
		  "TT",
		  "U",
		  "UL",
		  "VAR",
		  );

sub define_element {
    local ($element) = @_;
    $Begin{$element} = "Noop";
    $End{$element} = "Noop";
}

foreach $element (@HTML_elements) {&define_element($element);}

# do nothing
sub Noop {
    local ($element, $xxx) = @_;
}

# called when a tag begins.  Dispatches using Begin
sub html_begin {
    local ($element, $tag, %attributes) = @_;

    local ($routine) = $Begin{$element};
    if ($routine eq "") {
	print STDERR "Unknown HTML element $element ($tag) on line $line_count\n";}
    else {eval "&$routine;"}
}

# called when a tag ends.  Explicit is 0 if tag end is because of minimization
# not that you should care.
sub html_end {
    local ($element, $explicit) = @_;
    local ($routine) = $End{$element};
    if ($routine eq "") {
	print STDERR "Unknown HTML element \"$element\" (END $explicit) on line $line_count\n";}
    else {eval "&$routine(\"$element\", $explicit)";}
}

sub html_content {
    local ($word) = @_;
}

sub html_whitespace {
    local ($whitespace) = @_;
}

sub html_comment {
    local ($tag) = @_;
}

# redefine these for application-specific initialization and cleanup

sub html_begin_doc {}

sub html_end_doc {}

# return a "true value" when loaded by perl.
1;

