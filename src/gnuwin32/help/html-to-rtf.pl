# Perl script to convert a HTML Help project to a Windows Help project
#
# based on hh2rtf by Steve Atkins at
#
# http://www.blighty.com/products/hh2rtf/  which says
#   `Freeware. You may not resell it or claim you wrote it. 
#    You can use it for anything, commercial or otherwise.'
#
# Modifications for R (C) 1999 B. D. Ripley
#
# Restriction: tables are hardwired for arguments etc.
#

$debug = 0;

# get directory where this file is.  
{
    $0 =~ /^(.*)\/.*$/;
    $my_dir = $1; 
    if ($my_dir !~ ?^/?) {
	$my_dir = $ENV{PWD} . "/" . $my_dir;
    }
    if ($my_dir =~ ?/$?) {
	chop ($my_dir);
    }
}

push(@INC, $my_dir);

# Parse command line arguments.
$HHC = "";

while ($#ARGV >=0) {
    $arg = shift;
    if ($arg =~ /^-./) {
	if ($arg =~ /-verbose/) {
#	    if ($#ARGV == -1) {die "Missing value for $arg\nStopped ";}
	    $debug = 1;
	} else {
	    die "Unrecognized switch $arg.\nStopped";}
    } else {
	$HHP = $arg;
    }
}

if ($HHP eq "") {
    die "Missing argument (HHP input file)\n";
}

$basename = $HHP;
if($HHP !~ /\.hhp$/) {
    print STDERR "$HHP doesn't have a .hhp suffix?\n";
} else {
    $basename =~ s/\.hhp$//;
}

$HPJ = ">" . $basename . ".hpj";
$RTF = ">" . $basename . ".rtf";
$CNT = ">" . $basename . ".cnt";
$HLP = $basename . ".hlp";
$BAT = ">convert.bat";

require "parse-html.pl" || die "Could not load parse-html.pl";
require "html-rtf.pl" || die "Could not load html-rtf.pl";

open HHP or die "Can't open Html Help Workshop file $HHP for input: $!\n";

$infiles = 0;
$language = "0x409 English (United States)";
while(<HHP>) {
    if(/Compiled file=(.*)$/) {
	$outhtm = $1;
	chomp $outhtm;
    }
    if(/Contents file=(.*)$/) {
	$contentshtm = $1;
	chomp $contentshtm;
    }
    if(/Default topic=(.*)$/) {
	$defaulthtm = $1;
	chomp $defaulthtm;
    }
    if(/Language=(.*)$/) {
	$language = $1;
	chomp $language;
    }
    if(/\[/) {
	$infiles = 0;
    }
    if(/\[FILES\]/) {
	$infiles = 1;
    }
    if($infiles) {
	chomp;
	if(length > 0 && !/\[/) {
#	    print "Got a file: $_\n";
	    $filehash{$_}++;
	}
    }
}
close HHP;

$language =~ s/ / 0x0 0x0 \;/o;

open HPJ or die "Can't open help project file $HPJ for output: $!\n";

print HPJ "[OPTIONS]\n";
print HPJ "HCW=0\n";
print HPJ "LCID=$language\n";
print HPJ "REPORT=Yes\n";
print HPJ "HLP=.\\$HLP\n\n";
print HPJ "CNT=.\\$basename.cnt\n";
print HPJ "COMPRESS=true\n";
print HPJ "TITLE=Help for Package $basename\n";
print HPJ "[CONFIG]\nBrowseButtons()\n";
print HPJ "[FILES]\n";
print HPJ ".\\$basename.rtf\n";

@filelist = sort keys %filehash;
$numfiles = @filelist;
close HPJ;

# set up browse sequence
sub browseorder {lc($a) cmp lc($b) or $a cmp $b;}
$seq=1000;
$browse{"Contents"} = $seq;
foreach $file (sort browseorder @filelist) {
    $kw = $file;
    $kw =~ s/\.html$//o;
    if($kw ne "00Index") { 
	$seq += 1; 
	$browse{$kw} = $seq; 
    }
}


if(defined $contentshtm) {
    print STDERR "Translating $contentshtm to $basename.cnt\n";
    open CHTM, $contentshtm or die "Can't open contents file $contentshtm for input: $!\n";
    open CNT or die "Can't open contents file $basename.cnt for output: $!\n";
    print CNT ":Base $basename.hlp\n";
    $level = 0;
    while(<CHTM>) {
	if(/<UL>/) {
	    if(defined $name) {
		print CNT "$level $name\n";
#		$level++;
#		$a = $level . " " . $name . "=_label_" . $local . "\n";
#		$level--;
	    }
	    $level++;
	}
	if(/<\/UL>/) {
	    $level--;
	}
	if(/<param name=\"Name\" value=\"([^\"]+)\"/) {
	    $name = $1;
	}
	if(/<param name=\"Local\" value=\"([^\"]+)\"/) {
	    $local = $1;
	    print CNT $level . " " . $name . "=_label_" . $local . "\n";
	}
    }
    close CHTM;
    close CNT;
}

print STDERR "Processing $numfiles topic files\n";

open RTF or die "Can't open RTF output file $basename.rtf: $!\n";

print RTF "{\\rtf1 \\ansi\n";
print RTF "\\deff0\n";
print RTF "{\\fonttbl\n";
print RTF "{\\f0\\froman Times New Roman;}\n";
print RTF "{\\f1\\fmodern Courier New;}\n";
print RTF "{\\f2\\froman Symbol;}\n";
print RTF "}\n";
print RTF "{\\colortbl\n";
print RTF "\\red0\\green0\\blue0;\n";
print RTF "\\red0\\green0\\blue220;\n";
print RTF "\\red0\\green255\\blue255;\n";
print RTF "\\red0\\green255\\blue0;\n";
print RTF "\\red255\\green0\\blue255;\n";
print RTF "\\red255\\green0\\blue0;\n";
print RTF "\\red255\\green255\\blue0;\n";
print RTF "\\red255\\green255\\blue255;\n";
print RTF "\\red0\\green0\\blue128;\n";
print RTF "\\red0\\green128\\blue128;\n";
print RTF "\\red0\\green128\\blue0;\n";
print RTF "\\red128\\green0\\blue128;\n";
print RTF "\\red128\\green0\\blue0;\n";
print RTF "\\red128\\green128\\blue0;\n";
print RTF "\\red102\\green102\\blue102;\n";
print RTF "\\red150\\green150\\blue150;\n";
print RTF "}\n";

foreach $file (@filelist) {
#    if ($file =~ /^Hershey/) {printf "skipping\n"; next;}
    if ($file =~ /^Japanese/) {printf "skipping Japanese\n"; next;}
    print STDERR "Translating $file\n";
    &parse_html($file);
    print RTF "\n\\page\n";
}

foreach $p (keys %popuplist) {
    print RTF "#{\\footnote _popup_$p}\n";
    print RTF $Font{"P"}, " ", $popuplist{$p};
    print RTF"\n\\page\n";
}

print RTF "}\n";

open BAT or die "Couldn't open graphics translation file, convert.bat: $!\n";
foreach $f (keys %imagelist) {
    if($f =~ /(.*)\.([^.]+)/) {
	$base = $1;
	$suffix = $2;
	print BAT "${suffix}2bmp $base.$suffix $base.bmp\n";
    }
}
close BAT;

print STDERR "Graphics conversions written to convert.bat\n";
print STDERR "Written RTF to $basename.rtf\n";
print STDERR "Written Help Project file to $basename.hpj\n";
$nummissingfiles = keys %missingfiles;
if($nummissingfiles) {
    print STDERR "\n$nummissingfiles files missing from $HHP\n\nAdding";
    $HHPFIX = ">" . $HHP . "_fixed";
    open HHP or die "Couldn't open $HHP for output: $!\n";
    open HHPFIX or die "Couldn't open $HHP_fixed for output: $!\n";
    while(<HHP>) {
	print HHPFIX;
	if(/\[FILES\]/) {
	    foreach $f (keys %missingfiles) {
		print HHPFIX "$f\n";
		print STDERR " $f";
	    }
	}
    }
    close HHP;
    close HHPFIX;
    print STDERR "\n\nFixed version written to ${HHP}_fixed\n";
}



