#-*- perl -*-

## Copyright (C) 2001--2002 R Development Core Team
## Copyright (C) 2003-4       The R Foundation
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## A copy of the GNU General Public License is available via WWW at
## http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
## writing to the Free Software Foundation, Inc., 51 Franklin Street,
## Fifth Floor, Boston, MA 02110-1301  USA.
##
## Send any bug reports to r-bugs@r-project.org

## Usage: perl massage-Examples.pl pkgname files

## Given a list of files of the form .../.../<name>.R, produce one large
## file, i.e., write to stdout, concatenating the files together with
## 1) Putting a HEADER in front
## 2) Wrapping every file in order to be more order independent
## 3) appending a FOOTER ...

use File::Basename;

my $PKG = shift @ARGV;
my @Rfiles;
if(-d $ARGV[0]) {
    my $dir = $ARGV[0];
    opendir(DIR, $dir) or die "cannot opendir $dir: $!";
    my @files = sort grep { /\.R$/ } readdir(DIR);
    closedir(DIR);
    foreach my $file (@files) {
	push(@Rfiles, "$dir/$file");
    }
} else {
    @Rfiles = @ARGV;
}

### * Header
print <<_EOF_;
### * <HEADER>
###
attach(NULL, name = "CheckExEnv")
assign(".CheckExEnv", as.environment(2), pos = length(search())) # base
## add some hooks to label plot pages for base and grid graphics
setHook("plot.new", ".newplot.hook")
setHook("persp", ".newplot.hook")
setHook("grid.newpage", ".gridplot.hook")

assign("cleanEx",
       function(env = .GlobalEnv) {
	   rm(list = ls(envir = env, all.names = TRUE), envir = env)
           RNGkind("default", "default")
	   set.seed(1)
   	   options(warn = 1)
	   delayedAssign("T", stop("T used instead of TRUE"),
		  assign.env = .CheckExEnv)
	   delayedAssign("F", stop("F used instead of FALSE"),
		  assign.env = .CheckExEnv)
	   sch <- search()
	   newitems <- sch[! sch %in% .oldSearch]
	   for(item in rev(newitems))
               eval(substitute(detach(item), list(item=item)))
	   missitems <- .oldSearch[! .oldSearch %in% sch]
	   if(length(missitems))
	       warning("items ", paste(missitems, collapse=", "),
		       " have been removed from the search path")
       },
       env = .CheckExEnv)
assign("..nameEx", "__{must remake R-ex/*.R}__", env = .CheckExEnv) # for now
assign("ptime", proc.time(), env = .CheckExEnv)
grDevices::postscript("$PKG-Ex.ps")
assign("par.postscript", graphics::par(no.readonly = TRUE), env = .CheckExEnv)
options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"))
options(warn = 1)    
_EOF_

if($PKG eq "tcltk") {
    print "require('tcltk') || q()\n\n";
} elsif($PKG ne "base") {
    print "library('$PKG')\n\n";
}
print "assign(\".oldSearch\", search(), env = .CheckExEnv)\n";
print "assign(\".oldNS\", loadedNamespaces(), env = .CheckExEnv)\n";

### * Loop over all R files, and edit a few of them ...
foreach my $file (@Rfiles) {
    my $have_examples = 0;
    my $have_par = 0;
    my $have_contrasts = 0;
    my $nm;

    $nm = basename $file, (".R");
    $nm =~ s/[^- .a-zA-Z0-9]/./g;

    open(FILE, "< $file") or die "file $file cannot be opened";
    while (<FILE>) {
	$have_examples = 1
	    if ((/_ Examples _/o) || (/### \*+ Examples/));
	next if /^#/; # need to skip comment lines
	$have_par = 1 if (/[^a-zA-Z0-9.]par\(/o || /^par\(/o);
	$have_contrasts = 1 if /options\(contrasts/o;
    }
    close(FILE);
    if ($have_examples) {
	print "cleanEx(); ..nameEx <- \"$nm\"\n\n";
    }

    print "### * $nm\n\n";
    print "flush(stderr()); flush(stdout())\n\n";
    open(FILE, "< $file") or die "file $file cannot be opened";
    while (<FILE>) { print $_; }
    close(FILE);

    if($have_par) {
	## if there were 'par()' calls, now reset them:
	print "graphics::par(get(\"par.postscript\", env = .CheckExEnv))\n";
    }
    if($have_contrasts) {
	## if contrasts were set, now reset them:
	print "options(contrasts = c(unordered = \"contr.treatment\"," .
	    "ordered = \"contr.poly\"))\n";
    }

}

### * Footer
print <<_EOF_;
### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", env = .CheckExEnv),"\\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\\\(> \\\\)?### [*]+" ***
### End: ***
quit('no')
_EOF_
