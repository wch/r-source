#-*- perl -*-
# Copyright (C) 2001-10 R Development Core Team
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
# A copy of the GNU General Public License is available at
# http://www.r-project.org/Licenses/

# Send any bug reports to r-bugs@r-project.org

use Cwd;
use File::Find;
use Win32;

my $fn, $component, $path;
my $startdir=cwd();
my $RVER, $RVER0;
my $RW=$ARGV[0];
my $SRCDIR=$ARGV[1];
my $PERSONAL=$ARGV[2];

$SRCDIR =~ s+/+\\+g; # need DOS-style paths


open ver, "< ../../../VERSION";
$RVER = <ver>;
close ver;
$RVER =~ s/\n.*$//;
$RVER =~ s/Under .*$/Pre-release/;
$RVER0 = $RVER;
$RVER0 =~ s/ .*$//;

my $sRW = Win32::GetShortPathName($SRCDIR);

## 32-bit only
my $RK = "R32";

my %uuids;

## we could use Win32::Guidgen, but it is not normally installed
## so it is easier to use C code.

my $nc = 1;
open guidfile, "<uuids" or die "Cannot open file 'uuids'\n";
while (<guidfile>) {
    chomp;
    $uuids{$nc++} = $_;
}
close guidfile;
$nc = 1;

## for x64 add InstallerVersion="200" Platforms="x64"
## see http://blogs.msdn.com/astebner/archive/2007/08/09/4317654.aspx
## and change the product ....
## Or something like

##<Condition Message='This application is for x64 Windows.'>
##  VersionNT64
##</Condition>

## In Wix 3.0 the Version has to be something like 2.11.0.51588
## and the xmlns is 2006/wi

## ALLUSERS = 1 for per-machine, blank for default.
## http://wix.mindcapers.com/wiki/Allusers_Install_vs._Per_User_Install
## For non-elevation  see
## http://blogs.msdn.com/astebner/archive/2007/11/18/6385121.aspx
$elevate = "InstallPrivileges=\"limited\"" if $PERSONAL;
$allusers = "1" unless $PERSONAL;
open insfile, "> R.wxs" or die "Cannot open R.wxs\n";
print insfile <<END;
<?xml version="1.0" encoding="windows-1252"?>
<Wix xmlns="http://schemas.microsoft.com/wix/2003/01/wi">
  <Product Manufacturer="R Development Core Team" 
   Id="3AF9DA8E-B4DB-49B2-802D-A279667F44E5"
   Language="1033"
   Name="R $RVER for Windows"
   Version="$RVER" 
   UpgradeCode="309E663C-CA7A-40B9-8822-5D466F1E2AF9">
    <Package Id="????????-????-????-????-????????????" 
     Keywords="R $RVER for Windows Installer" 
     Description="R $RVER for Windows Installer" 
     Comments="R Language and Environment" 
     Manufacturer="R Development Core Team" 
     InstallerVersion="100" 
     Languages="1033" 
     Compressed="yes" 
     $elevate 
     SummaryCodepage="1252" />
    <Media Id='1' Cabinet='Sample.cab' EmbedCab='yes' DiskPrompt="CD-ROM #1" />
    <Property Id='DiskPrompt' Value="R for Windows Installation [1]" />
    <Property Id="ALLUSERS">$allusers</Property>

    <Directory Id='TARGETDIR' Name='SourceDir'>

      <Directory Id='ProgramFilesFolder' Name='PFiles'>
        <Directory Id='R' Name='R'>
          <Directory Id='INSTALLDIR' Name = '$sRW' LongName='$RW'>
END
## The standard folder names are listed at
## http://msdn.microsoft.com/en-us/library/aa372057.aspx
## They include ProgramFiles64Folder: this one is 32-bit

my $rgui, $rhelp;
my %comp;
open tfile, "<files.wxs" or die "Cannot open 'files.wxs'\n";
while(<tfile>) {
    next unless /^        /;
    if(/<Component Id=\"([^\"]*)\"/) {
	$id = $1;
    }
    ## tallow in WiX 2.0.4221 uses 'src', in 2.0.5805 uses 'Source'
    if(/<File Id=\"([^\"]*).* (src|Source)=\"([^\"]*)\"/) {
	$fn = $1;
	$src = $3;
	$src =~ s+.*\\$SRCDIR\\++;
	$src =~ s+\\+/+g;
	$comp{$src} = $id;
	$rgui = "$fn" if $src eq "bin/i386/Rgui.exe";
	$rhelp = "$fn" if $src eq "doc/html/index.html";
    }
    if(/PUT-GUID-HERE/) {
	s/PUT-GUID-HERE/$uuids{$nc++}/;
    }
    print insfile "    ", $_;
}
close tfile;

$path="${SRCDIR}";chdir($path);
my %main;

find(\&listFiles, ".");

## Note: Root="HKMU" resolves to HKLM for an all-users install,
## HKCU for a personal one
## http://wix.mindcapers.com/wiki/HKMU_registry_key

print insfile <<END;
          </Directory>
        </Directory>
      </Directory>

      <Directory Id="StartMenuFolder" Name="SMenu">
        <Directory Id="ProgramMenuFolder" Name="Programs">
          <Directory Id="RMENU" Name="R">
            <Component Id="shortcut0" 
             Guid="$uuids{$nc++}" KeyPath="yes">
              <Shortcut Id="RguiStartMenuShortcut" Directory="RMENU" Name="R" 
               LongName="R $RVER" Target="[!$rgui]" 
               WorkingDirectory="INSTALLDIR" />
            </Component>
            <Component Id="shortcut1" 
             Guid="$uuids{$nc++}" KeyPath="yes">
              <Shortcut Id="HelpStartMenuShortcut" Directory="RMENU" 
               Name="RHelp" LongName="R $RVER Help" Target="[!$rhelp]" 
               WorkingDirectory="INSTALLDIR" />
            </Component>
            <Component Id="shortcut2" 
             Guid="$uuids{$nc++}" KeyPath="yes">
              <Shortcut Id="UninstallStartMenuShortcut" Directory="RMENU" 
               Name="RUninst" LongName="Uninstall R $RVER" 
               Target="[SystemFolder]\msiexec.exe" 
               Arguments="/x [ProductCode]" Icon="shell32.dll" 
               IconIndex="32" WorkingDirectory="INSTALLDIR" />
            </Component>
          </Directory>
        </Directory>
      </Directory>
      <Directory Id="DesktopFolder" Name="Desktop">
        <Component Id="desktopshortcut0" DiskId="1" Guid="$uuids{$nc++}">
          <Shortcut Id="RguiDesktopShortcut" Directory="DesktopFolder" Name="R" LongName="R $RVER"
           WorkingDirectory="INSTALLDIR" Target="[!$rgui]" />
        </Component>
      </Directory>

      <Directory Id="AppDataFolder" Name="AppData">
        <Directory Id="Microsoft" Name="MS" LongName="Microsoft">
          <Directory Id="InternetExplorer" Name="IE" LongName="Internet Explorer">
            <Directory Id="QuickLaunch" Name="QLaunch" LongName="Quick Launch">
              <Component Id="quickshortcut0" DiskId="1" Guid="$uuids{$nc++}">
                <Shortcut Id="RguiQuickShortcut" Directory="QuickLaunch" Name="R" LongName="R $RVER"
                 WorkingDirectory="INSTALLDIR" Target="[!$rgui]" />
              </Component>
            </Directory>
          </Directory>
        </Directory>
      </Directory>


      <Component Id="registry0" Guid="$uuids{$nc++}">
        <Registry Id="RInstallPath" Root="HKMU" Key="Software\\R-core\\R" 
         Name="InstallPath" Type="string" KeyPath="yes" Value="[INSTALLDIR]" />
      </Component>
      <Component Id="registry7" Guid="$uuids{$nc++}">
        <Registry Id="RVerInstallPath" Root="HKMU" 
         Key="Software\\R-core\\R" Name="InstallPath"
         Type="string" KeyPath="yes" Value="[INSTALLDIR]" />
      </Component>
      <Component Id="registry1" Guid="$uuids{$nc++}">
        <Registry Id="RCurrentVersion" Root="HKMU" Key="Software\\R-core\\R" 
         Name="Current Version" Type="string" KeyPath="yes" 
         Value="[ProductVersion]" />
      </Component>
      <Component Id="registry2" Guid="$uuids{$nc++}">
        <Registry Id="RCurrentVerInstallPath" Root="HKMU" 
         Key="Software\\R-core\\R\\[ProductVersion]" Name="InstallPath"
         Type="string" KeyPath="yes" Value="[INSTALLDIR]" />
      </Component>
      <Component Id="registry20" Guid="$uuids{$nc++}">
        <Registry Id="RXInstallPath" Root="HKMU" Key="Software\\R-core\\${RK}" 
         Name="InstallPath" Type="string" KeyPath="yes" Value="[INSTALLDIR]" />
      </Component>
      <Component Id="registry21" Guid="$uuids{$nc++}">
        <Registry Id="RXVerInstallPath" Root="HKMU" 
         Key="Software\\R-core\\${RK}" Name="InstallPath"
         Type="string" KeyPath="yes" Value="[INSTALLDIR]" />
      </Component>
      <Component Id="registry22" Guid="$uuids{$nc++}">
        <Registry Id="RXCurrentVersion" Root="HKMU" Key="Software\\R-core\\${RK}" 
         Name="Current Version" Type="string" KeyPath="yes" 
         Value="[ProductVersion]" />
      </Component>
      <Component Id="registry23" Guid="$uuids{$nc++}">
        <Registry Id="RXCurrentVerInstallPath" Root="HKMU" 
         Key="Software\\R-core\\${RK}\\[ProductVersion]" Name="InstallPath"
         Type="string" KeyPath="yes" Value="[INSTALLDIR]" />
      </Component>

      <Component Id="registry3" Guid="$uuids{$nc++}">
        <Registry Id="RData" Root="HKCR" Key=".RData" Type="string"
         KeyPath="yes" Value="RWorkspace" />
      </Component>
      <Component Id="registry4" Guid="$uuids{$nc++}">
        <Registry Id="RWorkspace" Root="HKCR" Key="RWorkspace" Type="string" 
         KeyPath="yes" Value="R Workspace" />
      </Component>
      <Component Id="registry5" Guid="$uuids{$nc++}">
        <Registry Id="RDataCommand" Root="HKCR" 
         Key="RWorkspace\\shell\\open\\command" Type="string" KeyPath="yes" 
         Value="&quot;[!$rgui]&quot; &quot;%1&quot;" />
      </Component>
      <Component Id="registry6" Guid="$uuids{$nc++}">
        <Registry Id="RDataDefaultIcon" Root="HKCR" 
         Key="RWorkspace\\DefaultIcon" Type="string" KeyPath="yes" 
         Value="[!$rgui],0" />
      </Component>
    </Directory>

    <Feature Id="main" Title="Main Files" Description="Main Files" Level="1" 
     ConfigurableDirectory="INSTALLDIR"
     Display="expand" InstallDefault="local" AllowAdvertise="no" 
     Absent="disallow">
END
    
foreach $n (sort values %main) {
    print insfile "      <ComponentRef Id='$n' />\n";
}

print insfile <<END;
    </Feature>
    <Feature Id="html" Title="HTML Manuals" Description="HTML versions of the manuals" Level="1" InstallDefault="local" AllowAdvertise="no">
END

foreach $n (sort values %html) {
    print insfile "      <ComponentRef Id='$n' />\n";
}

print insfile <<END;
    </Feature>

    <Feature Id="manuals" Title="On-line PDF Manuals" 
     Description="On-line PDF Manuals" Level="1"
     InstallDefault="local" AllowAdvertise="no" Display="expand">
END

foreach $n (sort values %manuals) {
    print insfile "      <ComponentRef Id='$n' />\n";
}

print insfile <<END;

      <Feature Id="manualsb" Title="Basic Manuals" 
       Description="Basic manuals in PDF" Level="1"
       InstallDefault="local" AllowAdvertise="no">
END

foreach $n (sort values %manualsb) {
    print insfile "      <ComponentRef Id='$n' />\n";
}
print insfile <<END;
      </Feature>

      <Feature Id="manualst" Title="Technical Manuals" 
       Description="Technical manuals in PDF" Level="1"
       InstallDefault="local" AllowAdvertise="no">
END

foreach $n (sort values %manualst) {
    print insfile "      <ComponentRef Id='$n' />\n";
}

print insfile <<END;
      </Feature>

      <Feature Id="refman" Title="Reference Manual" 
       Description="Reference Manual (help pages in PDF)" Level="1000"
       InstallDefault="local" AllowAdvertise="no">
END

foreach $n (sort values %refman) {
    print insfile "      <ComponentRef Id='$n' />\n";
}

print insfile <<END;
      </Feature>

      <Feature Id="libdocs" Title="Docs for Packages grid and Matrix" Description="Docs for packages grid and Matrix: mainly PDF vignettes and their sources and code" Level="1000"
       InstallDefault="local" AllowAdvertise="no">
END

foreach $n (sort values %libdocs) {
    print insfile "      <ComponentRef Id='$n' />\n";
}

print insfile <<END;
      </Feature>
    </Feature>

    <Feature Id="tcl" Title="Support Files for Package tcltk" Description="A binary distribution of Tcl/Tk for use by R package tcltk" Level="1"
     InstallDefault="local" AllowAdvertise="no" Display="expand">
END

foreach $n (sort values %tcl) {
    print insfile "      <ComponentRef Id='$n' />\n";
}


print insfile <<END;
      </Feature>

      <Feature Id="tcl1" Title="Tcl/Tk Help (Compiled HTML)" Description="Tcl/Tk Help (Compiled HTML)" Level="1000"
       InstallDefault="local" AllowAdvertise="no">
END

foreach $n (sort values %tcl1) {
    print insfile "      <ComponentRef Id='$n' />\n";
}

print insfile <<END;

      <Feature Id="tcl2" Title="Timezone files for Tcl" Description="Timezone files for Tcl" Level="1000"
       InstallDefault="local" AllowAdvertise="no">
END

foreach $n (sort values %tcl2) {
    print insfile "      <ComponentRef Id='$n' />\n";
}

print insfile <<END;
      </Feature>
    </Feature>

    <Feature Id="trans" Title="Message Translations" Description="Messages translated to other languages" Level="1"
     InstallDefault="local" AllowAdvertise="no">
END

foreach $n (sort values %trans) {
    print insfile "      <ComponentRef Id='$n' />\n";
}

print insfile <<END;
    </Feature>

    <Feature Id="tests" Title="Test files" Description="Test files" Level="1"
     InstallDefault="local" AllowAdvertise="no">
END

foreach $n (sort values %tests) {
    print insfile "      <ComponentRef Id='$n' />\n";
}


## look up local Windows system32 directory
my $WINDOWS = Win32::GetFolderPath(Win32::CSIDL_SYSTEM);

print insfile <<END;
    </Feature>

    <Feature Id="shortcuts" Title="Shortcuts" Description="Shortcut install options" Level="1" InstallDefault="local"
     AllowAdvertise='no' Display="expand">
      <Feature Id='sshortcuts' Title='Start Menu Shortcuts' Description='Install Start menu shortcuts' Level='1'
       ConfigurableDirectory='RMENU' InstallDefault='local' AllowAdvertise='no'>
        <ComponentRef Id='shortcut0' />
        <ComponentRef Id='shortcut1' />
        <ComponentRef Id='shortcut2' />
      </Feature>
      <Feature Id='dshortcut' Title='Desktop Shortcut' Description='Install Desktop shortcut' Level='1'
       InstallDefault='local' AllowAdvertise='no'>
        <ComponentRef Id='desktopshortcut0' />
      </Feature>
      <Feature Id="qshortcut" Title="Quicklaunch Shortcut" Description="Install Quick Launch shortcut" Level="1000"
       InstallDefault="local" AllowAdvertise="no">
        <ComponentRef Id='quickshortcut0' />
      </Feature>
    </Feature>
    <Feature Id="registryversion" Title="Save Version in Registry"
     Description="Save the R version and install path in the Registry" Level="1" InstallDefault="local" AllowAdvertise="no">
      <ComponentRef Id='registry1' />
      <ComponentRef Id='registry7' />
      <ComponentRef Id='registry2' />
      <ComponentRef Id='registry20' />
      <ComponentRef Id='registry21' />
      <ComponentRef Id='registry22' />
      <ComponentRef Id='registry23' />
    </Feature>
    <Feature Id="associate" Title="Associate with .RData files"
     Description="Associate R with .RData files" Level="1" InstallDefault="local" AllowAdvertise="no">
      <ComponentRef Id='registry3' />
      <ComponentRef Id='registry4' />
      <ComponentRef Id='registry5' />
      <ComponentRef Id='registry6' />
    </Feature>

    <UIRef Id="WixUI_FeatureTree" />
    <UIRef Id="WixUI_ErrorProgressText" />

    <Icon Id="shell32.dll" SourceFile="$WINDOWS\\shell32.dll" />

  </Product>
</Wix>
END
## maybe better to use "WixUI_FeatureTree": always custom

close insfile;

sub listFiles {
    $fn = $File::Find::name;
    if (!(-d $_)) {
	$_ = $dir = $fn;
	s+^./++;
	s+/+\\+g;
	$fn =~ s+.*/++g;

	## These manuals are on the Rgui menu, so should always be installed
	if ($_ eq "doc\\manual\\R-FAQ.html"
		 || $_ eq "doc\\html\\rw-FAQ.html"
		 || $_ eq "share\\texmf\\Sweave.sty") {
	    $component = "main";
	} elsif (m/^doc\\html/
		 || m/^library\\[^\\]*\\html/
		 || $_ eq "library\\R.css") {
	    $component = "main";
	} elsif (m/^doc\\manual\\[^\\]*\.html/ ) {
	    $component = "html";
	} elsif ($_ eq "doc\\manual\\R-data.pdf"
		 || $_ eq "doc\\manual\\R-intro.pdf") {
	    $component = "manuals/basic";
	} elsif ($_ eq "doc\\manual\\R-admin.pdf" 
		 || $_ eq "doc\\manual\\R-exts.pdf"
		 || $_ eq "doc\\manual\\R-ints.pdf"
		 || $_ eq "doc\\manual\\R-lang.pdf") {
	    $component = "manuals/technical";
	} elsif ($_ eq "doc\\manual\\refman.pdf") {
	    $component = "manuals/refman";
	} elsif (m/^doc\\manual/ && $_ ne "doc\\manual\\R-FAQ.pdf") {
	    $component = "manuals";
	} elsif (m/^library\\[^\\]*\\tests/) {
	    	$component = "tests";
	} elsif (m/^tests/) {
	    	$component = "tests";
	} elsif (m/^Tcl\\doc\\.*chm$/) {
	    $component = "tcl/chm";
	} elsif (m/^Tcl\\lib\\tcl8.5\\tzdata/) {
	    $component = "tcl/tzdata";
	} elsif (m/^Tcl/) {
	    $component = "tcl";
	} elsif (m/^library\\grid\\doc/ || m/^library\\Matrix\\doc/) {
	    $component = "libdocs";
	} elsif (m/^share\\locale/ 
		 || m/^library\\[^\\]*\\po/) {
	    $component = "trans";
	} else {
	    $component = "main";
	}

	s+\\+/+g;
	$ncomp = $comp{$_};
	$main{$_} = $ncomp if $component eq "main";
	$html{$_} = $ncomp if $component eq "html";
	$manuals{$_} = $ncomp if $component eq "manuals";
	$manualsb{$_} = $ncomp if $component eq "manuals/basic";
	$manualst{$_} = $ncomp if $component eq "manuals/technical";
	$refman{$_} = $ncomp if $component eq "manuals/refman";
	$libdocs{$_} = $ncomp if $component eq "libdocs";
	$tcl{$_} = $ncomp if $component eq "tcl";
	$tcl1{$_} = $ncomp if $component eq "tcl/chm";
	$tcl2{$_} = $ncomp if $component eq "tcl/tzdata";
	$trans{$_} = $ncomp if $component eq "trans";
	$tests{$_} = $ncomp if $component eq "tests";
    }
}
