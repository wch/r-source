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

    my $retval = "<HEAD><TITLE>R: $title</TITLE></HEAD>\n" .
	"<BODY TEXT=\"#000000\" BGCOLOR=\"#FFFFFF\" " .
	"LINK=\"#0000F0\" VLINK=\"#660066\" ALINK=\"#FF0000\" " .
	"BACKGROUND=\"white\">\n" .
	"<h1 align=center>\n" .
        "<FONT FACE=\"Courier New,Courier\" COLOR=\"#999999\" " .
        "size=+3><b>\n" .
        "$title\n" .
        "</b></FONT><img src=\"$top/logo.jpg\" align=center></h1>\n\n" .
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
    my $title = $_[0];

    my $retval = "<HEAD><TITLE>R: $title</TITLE></HEAD>\n" .
	"<BODY TEXT=\"#000000\" BGCOLOR=\"#FFFFFF\" " .
	"LINK=\"#0000F0\" VLINK=\"#660066\" ALINK=\"#FF0000\" " .
	"BACKGROUND=\"white\">\n\n";
    
    $retval .= html_title2($title);

}

sub html_functionfoot
{
    my $retval;
    
    if($HTML){
	$retval .= "\n\n<p align=center><hr><div align=center>" .
	    "<a href=\"00Index.$HTML\">[Package Contents]</a>\n\n";
    }

    $retval .= "</BODY></HTML>\n";
}

# A Title of level 2

sub html_title2
{
    my $title = $_[0];

    "<h2 align=center><FONT FACE=\"Courier New,Courier\" " .
	"COLOR=\"#999999\">$title</FONT></h2>\n\n";
}

sub html_title3
{
    my $title = $_[0];

    "<h2><FONT FACE=\"Courier New,Courier\" " .
	"COLOR=\"#666666\">$title</FONT></h2>\n\n";
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




