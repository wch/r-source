# Subroutines for converting R documentation into HTML

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
        "<div align=\"center\">\n";

    $retval .= "<A HREF=\"$prev\"><IMG SRC=\"$top/left.jpg\"\n" .
	"ALT=\"[$prevtext]\" WIDTH=\"30\" HEIGHT=\"30\" BORDER=\"0\"></A>\n"
	    if $prev;

    $retval .=
	"<A HREF=\"$up\"><IMG SRC=\"$top/up.jpg\"" .
        "ALT=\"[$uptext]\" WIDTH=\"30\" HEIGHT=\"30\" BORDER=\"0\"></A>\n"
	    if $up;

    $retval .= "<A HREF=\"$next\"><IMG SRC=\"$top/right.jpg\"\n" .
    "ALT=\"[$nextext]\" WIDTH=\"30\" HEIGHT=\"30\" BORDER=\"0\"></A>\n"
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
	$retval .= "<TABLE WIDTH=\"100%\"><TR>" .
	    "<TD>$name {$pkgname}</TD>" .
	    "<TD ALIGN=\"right\">R Documentation</TD></TR></TABLE>";
    }

    $retval .= html_title2($title);
}

sub html_functionfoot
{
    my $retval;

    if($HTML){
	$retval .= "\n\n<hr><div align=\"center\">" .
	    "<a href=\"00Index.$HTML\">[Package Contents]</a></div>\n\n";
    }

    $retval .= "</BODY></HTML>\n";
}

# A Title of level 2

sub html_title2
{
    my $title = $_[0];

    if($opt_chm) {
	"\n<h2 align=\"center\"><FONT COLOR=\"#0000FF\">" .
	    "<tt>$title</tt></FONT></h2>\n\n";
    } else {
	"\n<h2>$title</h2>\n\n";
    }
}

sub html_title3
{
    my $title = $_[0];

    if($opt_chm) {
	"\n<h3><FONT COLOR=\"#666666\">" .
	    "<tt>$title</tt></FONT></h3>\n\n";
    } else {
	"\n<h3>$title</h3>\n\n";
    }
}


## produce alphabet for head of pages
## optional argument gives array ofg letters to use
sub html_alphabet
{
    my @letters = @_;

    @letters = (A..Z) if $#letters<0;
    my $retval = "<p align=\"center\">\n";
    foreach $letter (sort(@letters)){
	$retval .= "<A HREF=\"#${letter}\">${letter}</A>\n";
    }
    $retval;
}

sub chm_pagehead
{
    my ($title) = @_;

    my $retval = "<HTML><HEAD><TITLE>$title</TITLE></HEAD>\n" .
	"<BODY TEXT=\"#000000\" BGCOLOR=\"#FFFFFF\" " .
	"LINK=\"#0000F0\" VLINK=\"#660066\" ALINK=\"#FF0000\" " .
	"BACKGROUND=\"white\">\n" .
	"<h1 align=\"center\">\n" .
        "<FONT COLOR=\"#999999\" " .
        "size=\"+3\"><tt>\n" .
        "$title\n" .
        "</tt></FONT><img src=\"logo.jpg\" alt=\"[R logo]\" align=\"center\"></h1>\n\n" .
        "<hr>\n\n";
    $retval .= "<OBJECT TYPE=\"application/x-oleobject\" CLASSID=\"clsid:1e2a7bd0-dab9-11d0-b93a-00c04fc99f9e\">\n";
    $retval .= "<PARAM NAME=\"Keyword\" VALUE=\".. Contents\">\n" .
	"</OBJECT>\n\n";
    $retval;
}

sub chm_functionhead
{
    my ($title, $pkgname, $name) = @_;

    my $retval = "<HTML><HEAD><TITLE>$title</TITLE></HEAD>\n" .
	"<BODY TEXT=\"#000000\" BGCOLOR=\"#FFFFFF\" " .
	"LINK=\"#0000F0\" VLINK=\"#660066\" ALINK=\"#FF0000\" " .
	"BACKGROUND=\"white\">\n\n";

    if($pkgname){
	$retval .= "<TABLE WIDTH=\"100%\"><TR>" .
	    "<TD>$name($pkgname)</TD>" .
	    "<TD ALIGN=\"right\">R Documentation</TD></TR></TABLE>";
    }

    $retval .= "<OBJECT TYPE=\"application/x-oleobject\" CLASSID=\"clsid:1e2a7bd0-dab9-11d0-b93a-00c04fc99f9e\">\n";
    foreach(@aliases) {
#	print "alias: $_\n";
	$retval .= "<PARAM NAME=\"Keyword\" VALUE=\"R:   $_\">\n";
    }
    $title =~ s/\"/'/go;  #'
    $title =~ s/,//go;  # commas seem to give problems
    $retval .= "<PARAM NAME=\"Keyword\" VALUE=\" $title\">\n" .
	"</OBJECT>\n\n";
    $retval .= html_title2($title);

}

1;
