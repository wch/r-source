# The head of a html page (e.g. used for packages list)
# Arguments:
#  $title      of the page
#  $top        relative path to $R_HOME/doc/html (must be "." if we
#              already are there)
#  $up         filename of the upper file
#  $uptext     alternate text for upper file
#  $prev       filename of the previous file (for button bar)
#  $prevtext   alternate text for previous file
#  $next       filename of next file
#  $nextext    alternate text for next file
#
# If no previous or next button should be displayed, leave the
# corresponding filenames empty.

sub html_pagehead
{
    my ($title, $top, $up, $uptext, $prev, $prevtext, $next, $nextext) = @_;

    my $retval = "<HTML><HEAD><TITLE>R: $title</TITLE>" .
	"<LINK REL=STYLESHEET TYPE=\"text/css\" SRC=\"$top/R.css\">" .
	"</HEAD><BODY>\n" .
	"<h1>$title " .
	"<img class=toplogo src=\"$top/logo.jpg\" alt=\"[R logo]\"></h1>\n\n" .
        "<hr>\n\n" .
        "<div align=center>\n";

    $retval .= "<A HREF=\"$prev\"><IMG SRC=\"$top/left.jpg\"\n" .
	"ALT=\"[$prevtext]\" WIDTH=30 HEIGHT=30 BORDER=0></A>\n"
	    if $prev;
    
    $retval .=
	"<A HREF=\"$up\"><IMG SRC=\"$top/up.jpg\"" .
        "ALT=\"[$uptext]\" WIDTH=30 HEIGHT=30 BORDER=0></A>\n"
	    if $up;

    $retval .= "<A HREF=\"$next\"><IMG SRC=\"$top/right.jpg\"\n" .
    "ALT=\"[$nextext]\" WIDTH=30 HEIGHT=30 BORDER=0></A>\n"
	if $next;
    
    $retval .= "</DIV>\n\n";

    $retval;
}



# The header & footer of a function page

sub html_functionhead
{
    my ($title, $pkgname, $name) = @_;

    my $retval = "<HTML><HEAD><TITLE>R: $title</TITLE>\n" .
	"<LINK REL=STYLESHEET TYPE=\"text/css\" SRC=\"../../../doc/html/R.css\">" .
	"</HEAD><BODY>\n\n";

    if($pkgname){
	$retval .= "<table width=100%><tr>" .
	    "<td>$name($pkgname)</td>" .
	    "<td align=right>R Documentation</td></tr></table><p>";
    }
    
    $retval .= html_title2($title);
}

sub html_functionfoot
{
    my $retval;
    
    if($HTML){
	$retval .= "\n\n<p align=center><hr><div align=center>" .
	    "<a href=\"00Index.$HTML\">[Package Contents]</a></div>\n\n";
    }

    $retval .= "</BODY></HTML>\n";
}

# A Title of level 2

sub html_title2
{
    my $title = $_[0];

    "<h2>$title</h2>\n\n";
}

sub html_title3
{
    my $title = $_[0];

    "<h3>$title</h3>\n\n";
}

sub html_alphabet
{
    "<p align=center>\n"
    . "<A HREF=\"#A\">A</A>\n" 
    . "<A HREF=\"#B\">B</A>\n" 
    . "<A HREF=\"#C\">C</A>\n" 
    . "<A HREF=\"#D\">D</A>\n" 
    . "<A HREF=\"#E\">E</A>\n" 
    . "<A HREF=\"#F\">F</A>\n" 
    . "<A HREF=\"#G\">G</A>\n" 
    . "<A HREF=\"#H\">H</A>\n" 
    . "<A HREF=\"#I\">I</A>\n" 
    . "<A HREF=\"#J\">J</A>\n" 
    . "<A HREF=\"#K\">K</A>\n" 
    . "<A HREF=\"#L\">L</A>\n" 
    . "<A HREF=\"#M\">M</A>\n" 
    . "<A HREF=\"#N\">N</A>\n" 
    . "<A HREF=\"#O\">O</A>\n" 
    . "<A HREF=\"#P\">P</A>\n" 
    . "<A HREF=\"#Q\">Q</A>\n" 
    . "<A HREF=\"#R\">R</A>\n" 
    . "<A HREF=\"#S\">S</A>\n"
    . "<A HREF=\"#T\">T</A>\n" 
    . "<A HREF=\"#U\">U</A>\n" 
    . "<A HREF=\"#V\">V</A>\n" 
    . "<A HREF=\"#W\">W</A>\n" 
    . "<A HREF=\"#X\">X</A>\n" 
    . "<A HREF=\"#Y\">Y</A>\n" 
    . "<A HREF=\"#Z\">Z</A>\n"
    . "</p>\n";
}

1;




