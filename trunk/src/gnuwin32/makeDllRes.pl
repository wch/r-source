use R::Dcf;
use R::Utils;

my $DllName = $ARGV[0];
my $pkgname = $DllName;
my $pkgversion = "unknown";

if(-r "../DESCRIPTION"){
    $description = new R::Dcf("../DESCRIPTION");
} elsif(-r "../../DESCRIPTION"){
    $description = new R::Dcf("../../DESCRIPTION");
}
if($description->{"Version"}) {
    $pkgversion = $description->{"Version"};
}
if($description->{"Package"}) {
    $pkgname = $description->{"Package"};
}


print <<ENDF;
#include <windows.h>
#include "Rversion.h"

VS_VERSION_INFO VERSIONINFO
FILEVERSION R_FILEVERSION
PRODUCTVERSION 3,0,0,0
FILEFLAGSMASK 0x3L
FILEOS VOS__WINDOWS32
FILETYPE VFT_APP
BEGIN
    BLOCK "StringFileInfo"
    BEGIN
        BLOCK "040904E4"
        BEGIN
ENDF
print "            VALUE \"FileDescription\", \"DLL for R package `$pkgname'\\0\"\n";
print "            VALUE \"FileVersion\", \"$pkgversion\\0\"\n";
print <<ENDF;
            VALUE "Compiled under R Version", R_MAJOR "." R_MINOR " (" R_YEAR "-" R_MONTH "-" R_DAY ")\\0"
            VALUE "Project info", "http://www.r-project.org\\0"
        END
    END
    BLOCK "VarFileInfo"
    BEGIN
        VALUE "Translation", 0x409, 1252
    END
END
ENDF
