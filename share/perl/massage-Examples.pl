#-*- perl -*-

## Copyright (C) 2001 R Development Core Team
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
## writing to the Free Software Foundation, Inc., 59 Temple Place,
## Suite 330, Boston, MA  02111-1307  USA.
##
## Send any bug reports to r-bugs@r-project.org

## Usage: perl massage-Examples.pl pkgname files

## Given a list of files of the form .../.../<name>.R, produce one large
## file, i.e. write to stdout, `cat'ting the files together with
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

## 1) ---- Header ----
print <<_EOF_;
attach(NULL, name = "CheckExEnv")
assign(".CheckExEnv", as.environment(2), pos = length(search())) # base
## This plot.new() patch has no effect yet for persp();
## layout() & filled.contour() are now ok
assign("plot.new", function() { .Internal(plot.new())
		       pp <- par(c("mfg","mfcol","oma","mar"))
		       if(all(pp\$mfg[1:2] == c(1, pp\$mfcol[2]))) {
			 outer <- (oma4 <- pp\$oma[4]) > 0; mar4 <- pp\$mar[4]
			 mtext(paste("help(",..nameEx,")"), side = 4,
			       line = if(outer)max(1, oma4 - 1) else min(1, mar4 - 1),
			       outer = outer, adj=1, cex= .8, col="orchid")} },
       env = .CheckExEnv)
assign("cleanEx", function(env = .GlobalEnv) {
	rm(list = ls(envir = env, all.names = TRUE), envir = env)
	RNGkind("Wichmann-Hill", "default")
	assign(".Random.seed", c(0,rep(7654,3)), pos=1)
       },
       env = .CheckExEnv)
assign("..nameEx", "__{must remake R-ex/*.R}__", env = .CheckExEnv) #-- for now
assign("ptime", proc.time(), env = .CheckExEnv)
postscript("$PKG-Examples.ps")
assign("par.postscript", par(no.readonly = TRUE), env = .CheckExEnv)
options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"))
_EOF_

if($PKG eq "tcltk") {
    print "require('tcltk') || q()\n";
} elsif($PKG ne "base") {
    print "library('$PKG')\n";
}

## 2) ---- edit a few of these files:
foreach my $file (@Rfiles) {
    my $bf = basename $file, (".R");
    my $have_examples=0, $have_par=0, $have_contrasts=0, $nm;

    open FILE, "< $file" or die "file $file cannot be opened";
    while (<FILE>) {
	$have_examples = 1 if /_ Examples _/o;
	$have_par = 1 if (/[^a-zA-Z0-9.]par\(/o || /^par\(/o);
	$have_contrasts = 1 if /options\(contrasts/o;
    }
    close FILE;
    if ($have_examples) {
	$nm = $bf;
	$nm =~ s/[^- .a-zA-Z0-9]/./g;
	print "cleanEx(); ..nameEx <- \"$nm\"\n";
    }

    open FILE, "< $file" or die "file $file cannot be opened";
    while (<FILE>) { print $_; }
    close FILE;

    if($have_par) {
	## if there were 'par(..)' calls, now reset them:
	print "par(get(\"par.postscript\", env = .CheckExEnv))\n";
    }
    if($have_contrasts) {
	## if contrasts were set, now reset them:
	print "options(contrasts = c(unordered = \"contr.treatment\", ordered = \"contr.poly\"))\n";
    }

}

## 3) ---- Footer ----
print <<_EOF_;
cat("Time elapsed: ", proc.time() - get("ptime", env = .CheckExEnv),"\\n")
dev.off(); quit('no')
_EOF_

