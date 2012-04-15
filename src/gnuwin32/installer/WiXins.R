#  File src/gnuwin32/installer/WiXins.R
#
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

## Collection of notes about WiX usage

## for x64 add InstallerVersion="200" Platforms="x64"
## see http://blogs.msdn.com/astebner/archive/2007/08/09/4317654.aspx
## Or something like
##<Condition Message='This application is for x64 Windows.'>
##  VersionNT64
##</Condition>
## Actually seem to need both, and it is 'Platform' in WiX 3.0

## ALLUSERS = 1 for per-machine, blank for default.
## http://wix.mindcapers.com/wiki/Allusers_Install_vs._Per_User_Install
## For non-elevation  see
## http://blogs.msdn.com/astebner/archive/2007/11/18/6385121.aspx
## Also
## http://msdn.microsoft.com/en-us/library/aa367559%28VS.85%29.aspx

## The standard folder names are listed at
## http://msdn.microsoft.com/en-us/library/aa372057.aspx

## http://windows-installer-xml-wix-toolset.687559.n2.nabble.com/64-bit-and-32-bit-Registry-Keys-in-same-MSI-td4439679.html

.make_R.wxs <- function(RW, srcdir, personal = "0")
{
    have64bit <- file_test("-d", file.path(srcdir, "bin", "x64"))
    have32bit <- file_test("-d", file.path(srcdir, "bin", "i386"))

    personal <- personal == "1"
    ## need DOS-style paths
    srcdir0 <- srcdir
    srcdir <- gsub("/", "\\", srcdir, fixed = TRUE)

    Rver <- readLines("../../../VERSION")[1L]
    Rver <- sub("Under .*$", "Pre-release", Rver)
    SVN <- sub("Revision: ", "", readLines("../../../SVN-REVISION"))[1L]
    Rver0 <- paste(sub(" .*$", "", Rver), SVN, sep = ".")

    uuids <- readLines("uuids")
    nc <- 1
    guuids <- function() {x <- uuids[nc]; nc <<- nc + 1L; x}


    con <- file("R.wxs", "w")
    cat(file = con, sep = "\n",
        '<?xml version="1.0" encoding="windows-1252"?>',
        '<Wix xmlns="http://schemas.microsoft.com/wix/2006/wi">',
        '  <Product Manufacturer="R Core Team" ',
        '   Id="*"',
        '   Language="1033"',
        sprintf('   Name="R%s %s (via msi)"',
                ifelse(have64bit, " x64", ""),
                Rver),
        sprintf('   Version="%s"', Rver0),
        '   UpgradeCode="309E663C-CA7A-40B9-8822-5D466F1E2AF9">',
        '    <Package Id="*" ',
        sprintf('     Keywords="R %s Installer"', Rver),
        sprintf('     Description="R %s Installer"', Rver),
        '     Comments="R Language and Environment"',
        '     Manufacturer="R Core Team"',
        if (have64bit) '     InstallerVersion="200"'
        else '     InstallerVersion="100"',
        if (have64bit) '     Platform="x64"',
        '     Languages="1033"',
        '     Compressed="yes"',
        if (personal) 'InstallPrivileges="limited"',
        '     SummaryCodepage="1252" />',
        '    <Media Id=\'1\' Cabinet=\'Sample.cab\' EmbedCab=\'yes\' DiskPrompt="CD-ROM #1" />',
        '    <Property Id=\'DiskPrompt\' Value="R Installation [1]" />',
        if(personal)'   <Property Id="ALLUSERS"></Property>'
        else '    <Property Id="ALLUSERS">1</Property>',
        sprintf('    <Property Id="RVersion">%s</Property>', Rver),
        '    <Icon Id="icon.ico" SourceFile="..\\front-ends\\R.ico"/>',
        '    <Property Id="ARPPRODUCTICON" Value="icon.ico" />',
        '')

    if (have64bit) {
        cat(file = con, sep = "\n", "",
            "<Condition Message='This application is for x64 Windows.'>",
            "  VersionNT64", "</Condition>")
    }

    name0 <- paste('Name="', srcdir, '" />', sep = '')
    ff <- readLines('files.wxs', warn = FALSE)
    ff <- grep("^        ", ff, value = TRUE)
    rx1 <- ' *<Component Id="([^"]*)".*'
    rx2 <- ' *<File Id=\"([^\"]*).* (src|Source)=\"([^\"]*)\".*'
    rx3 <- paste(".*\\", srcdir, "\\", sep = "")
    comps <- ids <- nm <- character()
    rgui <- rgui64 <- rhelp <- 'unknown'
    comp <- id <- 'unknown'
    rx3 <- paste(".*", srcdir0, "/", sep="")
    rx3 <- ".*SourceDir/"
    for(i in seq_along(ff)) {
        f <- ff[i]
        if(grepl(rx1, f)) id <- sub(rx1, "\\1", f)
        if(grepl(rx2, f)) {
            fn <- sub(rx2, "\\1", f)
            src <- sub(rx3, "", sub(rx2, "\\3", f), fixed = TRUE)
            src <- gsub("\\", "/", src, fixed = TRUE)
            if(grepl("bin/i386/Rgui.exe$", src)) rgui <- fn
            if(grepl("bin/x64/Rgui.exe$", src)) rgui64 <- fn
            if(grepl("doc/html/index.html$", src)) rhelp <- fn
            ids <- c(ids, id)
            nm <- c(nm, src)
            g <- sub(rx3, "", src)
            component <- if (have64bit && grepl("^Tcl/(bin|lib)64", g)) "x64"
            else if (have64bit &&
                     (grepl("^Tcl/bin", g) ||
                      grepl("^Tcl/lib/(dde1.3|reg1.2|Tktable)", g))) "i386"
            else if (have64bit && grepl("/i386/", g)) "i386"
            else if (have64bit && grepl("/x64/", g)) "x64"
            else "main"
            comps <- c(comps, component)
        }
        if(grepl("PUT-GUID-HERE", f))
            f <- sub("PUT-GUID-HERE", guuids(), f, fixed = TRUE)
        f <- sub("SourceDir", srcdir, f, fixed = TRUE)
        f <- sub("TARGETDIR", "INSTALLDIR", f, fixed = TRUE)
        f <- sub(name0, "/>", f, fixed = TRUE)
        cat("    ", f, "\n", file=con, sep="")
    }
    if (rgui == "unknown") rgui <- rgui64


    cat(file = con, sep = "\n", '',
        '    <Directory Id=\'TARGETDIR\' Name=\'SourceDir\'>',
        '',
        ## empty components to work around bug in Windows Installer
        ## offering to install components with no features from the network
        sprintf('    <Component Id="dummyman" Guid="%s"></Component>', guuids()),
        sprintf('    <Component Id="dummytcl" Guid="%s"></Component>', guuids()),
        sprintf('    <Component Id="dummystart" Guid="%s"></Component>', guuids()),
        '',
        if (have64bit)
        '      <Directory Id=\'ProgramFiles64Folder\' Name=\'PFiles\'>'
        else
        '      <Directory Id=\'ProgramFilesFolder\' Name=\'PFiles\'>',
        "        <Directory Id='Rdir' Name='R'>",
        sprintf("         <Directory Id='INSTALLDIR' Name='%s'>", srcdir),
        "         </Directory>",
        "        </Directory>",
        "      </Directory>")

    cat(file = con, sep="\n",
'      <Directory Id="PersonalFolder" Name="Personal">',
'        <Directory Id="STARTDIR" Name="R"></Directory>',
'      </Directory>')

    cat(file = con, sep="\n",
'      <Directory Id="StartMenuFolder" Name="SMenu">',
'        <Directory Id="ProgramMenuFolder" Name="Programs">',
'          <Directory Id="RMENU" Name="R">')
    if (have32bit)
        cat(file = con, sep="\n",
sprintf('             <Component Id="shortcut0" Guid="%s">', guuids()),
'              <Shortcut Id="RguiStartMenuShortcut" Directory="RMENU"',
sprintf('               Name="R i386 %s" Target="[!%s]" ', Rver, rgui),
'               WorkingDirectory="STARTDIR" />',
            ## stop validation errors
'            <RegistryValue Root="HKCU" Key="Software\\R-core\\R" Name="installed" Type="integer" Value="1" KeyPath="yes"/>',
'            </Component>')
    if (have64bit)
        cat(file = con, sep="\n",
sprintf('             <Component Id="shortcut64" Guid="%s">', guuids()),
'              <Shortcut Id="Rgui64StartMenuShortcut" Directory="RMENU"',
sprintf('               Name="R x64 %s" Target="[!%s]" ', Rver, rgui64),
'               WorkingDirectory="STARTDIR" />',
            ## stop validation errors
'            <RegistryValue Root="HKCU" Key="Software\\R-core\\R" Name="installed" Type="integer" Value="1" KeyPath="yes"/>',
'            </Component>')

    cat(file = con, sep="\n",
sprintf('             <Component Id="shortcut1" Guid="%s">', guuids()),
'              <Shortcut Id="HelpStartMenuShortcut" Directory="RMENU"',
sprintf('               Name="R %s Help" Target="[!%s]"', Rver, rhelp),
'               WorkingDirectory="STARTDIR" />',
            ## The next two stop validation errors
'            <RemoveFolder Id="RMENU" On="uninstall"/>',
'            <RegistryValue Root="HKCU" Key="Software\\R-core\\R" Name="installed" Type="integer" Value="1" KeyPath="yes"/>',
'            </Component>',
'          </Directory>',
'        </Directory>',
'      </Directory>',
'      <Directory Id="DesktopFolder" Name="Desktop">')
    if (have32bit)
        cat(file = con, sep="\n",
sprintf('        <Component Id="desktopshortcut0" DiskId="1" Guid="%s">', guuids()),
sprintf('          <Shortcut Id="RguiDesktopShortcut" Directory="DesktopFolder" Name="R i386 %s"', Rver),
sprintf('           WorkingDirectory="STARTDIR" Target="[!%s]" />', rgui),
'            <RegistryValue Root="HKCU" Key="Software\\R-core\\R" Name="installed" Type="integer" Value="1" KeyPath="yes"/>',
'        </Component>')
    if (have64bit)
        cat(file = con, sep="\n",
sprintf('        <Component Id="desktopshortcut64" DiskId="1" Guid="%s">', guuids()),
sprintf('          <Shortcut Id="Rgui64DesktopShortcut" Directory="DesktopFolder" Name="R x64 %s"', Rver),
sprintf('           WorkingDirectory="STARTDIR" Target="[!%s]" />', rgui64),
'            <RegistryValue Root="HKCU" Key="Software\\R-core\\R" Name="installed" Type="integer" Value="1" KeyPath="yes"/>',
'        </Component>')
       cat(file = con, sep="\n",
'      </Directory>',
'',
'      <Directory Id="AppDataFolder" Name="AppData">',
'        <Directory Id="Microsoft" Name="Microsoft">',
'          <Directory Id="InternetExplorer" Name="Internet Explorer">',
'            <Directory Id="QuickLaunch" Name="Quick Launch">',
sprintf('              <Component Id="quickshortcut0" DiskId="1" Guid="%s">', guuids()),
sprintf('                <Shortcut Id="RguiQuickShortcut" Directory="QuickLaunch" Name="R %s"', Rver),
sprintf('                 WorkingDirectory="STARTDIR" Target="[!%s]" />', rgui),
'                <RegistryValue Root="HKCU" Key="Software\\R-core\\R" Name="installed" Type="integer" Value="1" KeyPath="yes"/>',
'              </Component>',
'            </Directory>',
'          </Directory>',
'        </Directory>',
'      </Directory>',
'',
''
)
    if(have32bit) { # go in 32-bit registry
    cat(file = con, sep="\n",
sprintf('      <Component Id="registry32" Guid="%s">', guuids()),
'        <RegistryKey Id="RInstallPath" Root="HKMU" Key="Software\\R-core\\R" Action="create">',
'         <RegistryValue Name="InstallPath" Type="string" Value="[INSTALLDIR]"/>',
'         <RegistryValue Name="Current Version" Type="string" Value="[RVersion]"/>',
'        </RegistryKey>',
'        <RegistryKey Id="RCurrentVerInstallPath" Root="HKMU" Key="Software\\R-core\\R\\[RVersion]" Action="createAndRemoveOnUninstall">',
'         <RegistryValue Name="InstallPath" Type="string" Value="[INSTALLDIR]"/>',
'        </RegistryKey>',
'        <RegistryKey Id="R32InstallPath" Root="HKMU" Key="Software\\R-core\\R32" Action="create">',
'         <RegistryValue Name="InstallPath" Type="string" Value="[INSTALLDIR]"/>',
'         <RegistryValue Name="Current Version" Type="string" Value="[RVersion]"/>',
'        </RegistryKey>',
'        <RegistryKey Id="R32CurrentVerInstallPath" Root="HKMU" Key="Software\\R-core\\R32\\[RVersion]" Action="createAndRemoveOnUninstall">',
'         <RegistryValue Name="InstallPath" Type="string" Value="[INSTALLDIR]"/>',
'        </RegistryKey>',
'      </Component>',
'')
}

    if(have64bit) { # go in 64-bit registry
    cat(file = con, sep="\n",
sprintf('      <Component Id="registry64" Guid="%s" Win64="yes">', guuids()),
'        <RegistryKey Id="Rx64InstallPath" Root="HKMU" Key="Software\\R-core\\R" Action="create">',
'         <RegistryValue Name="InstallPath" Type="string" Value="[INSTALLDIR]"/>',
'         <RegistryValue Name="Current Version" Type="string" Value="[RVersion]"/>',
'        </RegistryKey>',
'        <RegistryKey Id="Rx64CurrentVerInstallPath" Root="HKMU" Key="Software\\R-core\\R\\[RVersion]" Action="createAndRemoveOnUninstall">',
'         <RegistryValue Name="InstallPath" Type="string" Value="[INSTALLDIR]"/>',
'        </RegistryKey>',
'        <RegistryKey Id="R64InstallPath" Root="HKMU" Key="Software\\R-core\\R64" Action="create">',
'         <RegistryValue Name="InstallPath" Type="string" Value="[INSTALLDIR]"/>',
'         <RegistryValue Name="Current Version" Type="string" Value="[RVersion]"/>',
'        </RegistryKey>',
'        <RegistryKey Id="R64CurrentVerInstallPath" Root="HKMU" Key="Software\\R-core\\R64\\[RVersion]" Action="createAndRemoveOnUninstall">',
'         <RegistryValue Name="InstallPath" Type="string" Value="[INSTALLDIR]"/>',
'        </RegistryKey>',
'      </Component>',
'')
}

    ## file associations
    cat(file = con, sep="\n",
sprintf('      <Component Id="registry3" Guid="%s">', guuids()),
"        <ProgId Id='RWorkspace' Description='R Workspace'>",
"          <Extension Id='RData'>",
sprintf("           <Verb Id='open' Command='Open' TargetFile='%s' Argument='\"%%1\"'/>", rgui),
"          </Extension>",
"        </ProgId>",
'      </Component>')


    cat(file = con, sep="\n", '    </Directory>')


    ## the features.
    cat(file = con, sep="\n",
        '',
        '    <Feature Id="main" Title="Main Files" Description="Main Files" Level="1"',
        '     ConfigurableDirectory="INSTALLDIR"',
        '     InstallDefault="local" AllowAdvertise="no"',
        '     Absent="disallow">')
    for(id in ids[comps == 'main'])
        cat(file = con,
            "      <ComponentRef Id='", id, "' />\n", sep="")
    cat(file = con, '    </Feature>\n')

    cat(file = con, sep="\n",
        '',
        '    <Feature Id="startup" Title="Starting Directory" Description="Set starting directory for R shortcuts" Level="1"',
        '     ConfigurableDirectory="STARTDIR"',
        '     InstallDefault="local" AllowAdvertise="no"',
        '     Absent="disallow">',
        '     <ComponentRef Id="dummystart" />',
        '    </Feature>\n')

    if (have64bit && have32bit) {
    cat(file = con, sep="\n",
        '',
        '    <Feature Id="i386" Title="32-bit Files" Description="32-bit binary files" Level="1"',
        '     InstallDefault="local" AllowAdvertise="no">')
    for(id in ids[comps == 'i386'])
        cat(file = con,
            "      <ComponentRef Id='", id, "' />\n", sep="")
    cat(file = con, '    </Feature>\n')
    }

    if (have64bit) {
    cat(file = con, sep="\n",
        '',
        '    <Feature Id="x64" Title="64-bit Files" Description="64-bit binary files" Level="1"',
        if (!have32bit) '     Absent="disallow"',
        '     InstallDefault="local" AllowAdvertise="no">')
    for(id in ids[comps == 'x64'])
        cat(file = con,
            "      <ComponentRef Id='", id, "' />\n", sep="")
    cat(file = con, '    </Feature>\n')
    }

    cat(file = con, sep="\n",
        '',
        '    <Feature Id="shortcuts" Title="Shortcuts" Description="Shortcut install options" Level="1" InstallDefault="local"',
        '     AllowAdvertise="no" Display="expand">',
        "      <Feature Id='sshortcuts' Title='Start Menu Shortcuts' Description='Install Start menu shortcuts' Level='1'",
        "       ConfigurableDirectory='RMENU' InstallDefault='local' AllowAdvertise='no'>",
        if (have32bit) "        <ComponentRef Id='shortcut0' />",
        if (have64bit) "        <ComponentRef Id='shortcut64' />",
        "        <ComponentRef Id='shortcut1' />",
        "      </Feature>",
        "      <Feature Id='dshortcut' Title='Desktop Shortcut' Description='Install Desktop shortcut' Level='1'",
        "       InstallDefault='local' AllowAdvertise='no'>",
        if (have32bit) "        <ComponentRef Id='desktopshortcut0' />",
        if (have64bit) "        <ComponentRef Id='desktopshortcut64' />",
        "      </Feature>",
        '      <Feature Id="qshortcut" Title="Quicklaunch Shortcut" Description="Install Quick Launch shortcut" Level="1000"',
        '       InstallDefault="local" AllowAdvertise="no">',
        "        <ComponentRef Id='quickshortcut0' />",
        "      </Feature>",
        "    </Feature>")

    cat(file = con, sep="\n",
        '    <Feature Id="registryversion" Title="Save Version in Registry"',
        '     Description="Save the R version and install path in the Registry" Level="1" InstallDefault="local" AllowAdvertise="no">',
        if (have32bit) "      <ComponentRef Id='registry32' />",
        if (have64bit) "      <ComponentRef Id='registry64' />",
        "    </Feature>",
        '    <Feature Id="associate" Title="Associate with .RData files"',
        '     Description="Associate R with .RData files" Level="1" InstallDefault="local" AllowAdvertise="no">',
        "      <ComponentRef Id='registry3' />",
        "    </Feature>")

    cat(file = con, sep="\n",
        "",
        '    <UIRef Id="WixUI_FeatureTree" />',
        '    <UIRef Id="WixUI_ErrorProgressText" />',
        '    <WixVariable Id="WixUILicenseRtf" Value="License.rtf" />',
        '',
#        '    <Icon Id="shell32.dll" SourceFile="C:\\Windows\\system32\\shell32.dll" />',
        '',
        "  </Product>",
        "</Wix>")

    close(con)
}


args <- commandArgs(TRUE)
do.call(".make_R.wxs", as.list(args))
