#-*- perl -*-
# Copyright (C) 2001 R Development Core Team
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

# Send any bug reports to r-bugs@r-project.org

use Cwd;
use File::Find;

my $fn, $component, $path;
my $startdir=cwd();
my $RVER;
my $RW=$ARGV[0];
my $iconpars="WorkingDir: \"{app}\"" ;
## add to the target command line as in the next example
# my $iconpars="Parameters: \"--sdi\"; WorkingDir: \"{app}\"" ;

open ver, "< ../../../VERSION";
$RVER = <ver>;
close ver;
$RVER =~ s/\n.*$//;
$RVER =~ s/Under .*$/Pre-release/;

open insfile, "> Rsmall.iss" || die "Cannot open Rsmall.iss\n";
print insfile <<END;
[Setup]
AppName=R for Windows
AppVerName=R for Windows $RVER
AppPublisher=R Development Core Team
AppPublisherURL=http://www.r-project.org
AppSupportURL=http://www.r-project.org
AppUpdatesURL=http://www.r-project.org
AppVersion=${RVER}
DefaultDirName={pf}\\R\\${RW}
DefaultGroupName=R
AllowNoIcons=yes
AlwaysCreateUninstallIcon=yes
LicenseFile=${RW}\\COPYING
DisableReadyPage=yes
DisableStartupPrompt=yes
OutputDir=.
OutputBaseFilename=miniR
WizardSmallImageFile=R.bmp
UsePreviousAppDir=no
ChangesAssociations=yes
DiskSpanning=yes

[Types]
Name: "minimal"; Description: "Minimal user installation"
Name: "custom"; Description: "Custom installation"; Flags: iscustom

[Components]
Name: "main"; Description: "Main Files"; Types: minimal custom; Flags: fixed
Name: "chtml"; Description: "Compiled HTML Help Files"; Types: minimal custom
Name: "manuals"; Description: "On-line (PDF) Manuals"; Types: minimal custom

[Tasks]
Name: "desktopicon"; Description: "Create a &desktop icon"; GroupDescription: "Additional icons:"; MinVersion: 4,4

[Icons]
Name: "{group}\\R $RVER"; Filename: "{app}\\bin\\Rgui.exe"; $iconpars
Name: "{userdesktop}\\R $RVER"; Filename: "{app}\\bin\\Rgui.exe"; MinVersion: 4,4; Tasks: desktopicon; $iconpars

[Registry] 
Root: HKLM; Subkey: "Software\\R-core"; Flags: uninsdeletekeyifempty
Root: HKLM; Subkey: "Software\\R-core\\R"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\\R-core\\R"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"
Root: HKLM; Subkey: "Software\\R-core\\R"; ValueType: string; ValueName: "Current Version"; ValueData: "${RVER}"

Root: HKCR; Subkey: ".RData"; ValueType: string; ValueName: ""; ValueData: "RWorkspace"; Flags: uninsdeletevalue 
Root: HKCR; Subkey: "RWorkspace"; ValueType: string; ValueName: ""; ValueData: "R Workspace"; Flags: uninsdeletekey 
Root: HKCR; Subkey: "RWorkspace\\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\\bin\\RGui.exe,0" 
Root: HKCR; Subkey: "RWorkspace\\shell\\open\\command"; ValueType: string; ValueName: ""; ValueData: """{app}\\bin\\RGui.exe"" ""%1""" 

[Messages]
WelcomeLabel2=This will install [name/ver] on your computer.%n%nIt is strongly recommended that you close all other applications you have running before continuing. This will help prevent any conflicts during the installation process.%n%nThis is a minimal installation, not including HTML help etc..%n


[Files]
END

$path="${RW}";$component="main";chdir($path);
find(\&listFiles, ".");

chdir($startdir);
$path="${RW}ch\\${RW}";$component="chtml";chdir($path);
find(\&listFiles, ".");

chdir($startdir);
$path="${RW}d1\\${RW}";$component="manuals";chdir($path);
find(\&listFiles, ".");


close insfile;

sub listFiles {
    $fn = $File::Find::name;
    $fn =~ s+^./++;
    if (!(-d $_)) {
	$fn =~ s+/+\\+g;
	$dir = $fn;
	$dir =~ s/[^\\]+$//;
	$dir = "\\".$dir;
	$dir =~ s/\\$//;
	print insfile "Source: \"$path\\$fn\"; DestDir: \"{app}$dir\"; CopyMode: alwaysoverwrite; Components: $component\n";
    }
}
