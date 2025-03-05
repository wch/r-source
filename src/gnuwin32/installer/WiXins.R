#  File src/gnuwin32/installer/WiXins.R
#
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

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
    ## The layout of 64-bit Intel builds is different so that it matches
    ## previous versions of R which supported sub-architectures (32-bit and
    ## 64-bit Intel) and installing files for both at the same time.

    havex64 <- file_test("-d", file.path(srcdir, "bin", "x64"))

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

    aarch64 <- FALSE
    if (!havex64) { 
        fout <- system(
                    paste("file", shQuote(file.path(srcdir, "bin", "R.exe"))),
                    intern=TRUE)
        if (grepl("Aarch64", fout, fixed = TRUE)
            || grepl("ARM64", fout, fixed = TRUE))
            aarch64 <- TRUE
    }

    if (aarch64) {
       # To distinguish aarch64 version from x86_64 version installed on
       # Windows/aarch64 in ARP entries and in shortcuts
       RverA <- paste0("aarch64 ", Rver)
       dsuffix <- "-aarch64"
    } else {
       if (havex64)
           RverA <- paste0("x64 ", Rver)
       else
           RverA <- Rver
       dsuffix <- ""
    }

    con <- file("R.wxs", "w")
    cat(file = con, sep = "\n",
        '<?xml version="1.0" encoding="windows-1252"?>',
        '<Wix xmlns="http://schemas.microsoft.com/wix/2006/wi">',
        '  <Product Manufacturer="R Core Team" ',
        '   Id="*"',
        '   Language="1033"',
        sprintf('   Name="R %s (via msi)"', RverA),
        sprintf('   Version="%s"', Rver0),
        '   UpgradeCode="309E663C-CA7A-40B9-8822-5D466F1E2AF9">',
        '    <Package Id="*" ',
        sprintf('     Keywords="R %s Installer"', Rver),
        sprintf('     Description="R %s Installer"', Rver),
        '     Comments="R Language and Environment"',
        '     Manufacturer="R Core Team"',
        if (aarch64) '     InstallerVersion="500"'
        else '     InstallerVersion="200"',
        if (havex64) '     Platform="x64"'
        else if (aarch64) '    Platform="arm64"',
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

    if (havex64) {
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
    rgui <- rhelp <- 'unknown'
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
            if(havex64 && grepl("bin/x64/Rgui.exe$", src)) rgui <- fn
            if(!havex64 && grepl("bin/Rgui.exe$", src)) rgui <- fn
            if(grepl("doc/html/index.html$", src)) rhelp <- fn
            ids <- c(ids, id)
            nm <- c(nm, src)
            g <- sub(rx3, "", src)
            component <- if (havex64 && grepl("/x64/", g)) "x64"
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


    cat(file = con, sep = "\n", '',
        '    <Directory Id=\'TARGETDIR\' Name=\'SourceDir\'>',
        '',
        ## empty components to work around bug in Windows Installer
        ## offering to install components with no features from the network
        sprintf('    <Component Id="dummystart" Guid="%s"></Component>', guuids()),
        '',
        '      <Directory Id=\'ProgramFiles64Folder\' Name=\'PFiles\'>',
        sprintf("        <Directory Id='Rdir' Name='R%s'>", dsuffix),
        sprintf("         <Directory Id='INSTALLDIR' Name='%s'>", srcdir),
        "         </Directory>",
        "        </Directory>",
        "      </Directory>")

    cat(file = con, sep="\n",
'      <Directory Id="StartMenuFolder" Name="SMenu">',
'        <Directory Id="ProgramMenuFolder" Name="Programs">',
sprintf('          <Directory Id="RMENU" Name="R%s">', dsuffix))

    cat(file = con, sep="\n",
sprintf('            <Component Id="shortcut64" Guid="%s">', guuids()),
'              <Shortcut Id="RguiStartMenuShortcut" Directory="RMENU"',
sprintf('               Name="R %s" Target="[!%s]" ', RverA, rgui),
'               WorkingDirectory="PersonalFolder" Arguments="--cd-to-userdocs"/>',
            ## stop validation errors
'            <RegistryValue Root="HKCU" Key="Software\\R-core\\R" Name="installed" Type="integer" Value="1" KeyPath="yes"/>',
'            </Component>')

    cat(file = con, sep="\n",
sprintf('            <Component Id="shortcut1" Guid="%s">', guuids()),
'              <Shortcut Id="HelpStartMenuShortcut" Directory="RMENU"',
sprintf('               Name="R %s Help" Target="[!%s]"', RverA, rhelp),
'               WorkingDirectory="PersonalFolder" />',
            ## The next two stop validation errors
'            <RemoveFolder Id="RMENU" On="uninstall"/>',
'            <RegistryValue Root="HKCU" Key="Software\\R-core\\R" Name="installed" Type="integer" Value="1" KeyPath="yes"/>',
'            </Component>',
'          </Directory>',
'        </Directory>',
'      </Directory>',
'      <Directory Id="DesktopFolder" Name="Desktop">')

        cat(file = con, sep="\n",
sprintf('        <Component Id="desktopshortcut64" DiskId="1" Guid="%s">', guuids()),
sprintf('          <Shortcut Id="RguiDesktopShortcut" Directory="DesktopFolder" Name="R %s"', RverA),
sprintf('           WorkingDirectory="PersonalFolder" Target="[!%s]" Arguments="--cd-to-userdocs"/>', rgui),
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
sprintf('                <Shortcut Id="RguiQuickShortcut" Directory="QuickLaunch" Name="R %s"', RverA),
sprintf('                 WorkingDirectory="PersonalFolder" Target="[!%s]" Arguments="--cd-to-userdocs"/>', rgui),
'                <RegistryValue Root="HKCU" Key="Software\\R-core\\R" Name="installed" Type="integer" Value="1" KeyPath="yes"/>',
'              </Component>',
'            </Directory>',
'          </Directory>',
'        </Directory>',
'      </Directory>',
'',
''
)

    ## registry
    cat(file = con, sep="\n",
sprintf('      <Component Id="registry64" Guid="%s">', guuids()),
'        <RegistryKey Root="HKMU" Key="Software\\R-core\\R">',
'         <RegistryValue Name="InstallPath" Type="string" Value="[INSTALLDIR]"/>',
'         <RegistryValue Name="Current Version" Type="string" Value="[RVersion]"/>',
'        </RegistryKey>',
'        <RegistryKey Root="HKMU" Key="Software\\R-core\\R\\[RVersion]">',
'         <RegistryValue Name="InstallPath" Type="string" Value="[INSTALLDIR]"/>',
'        </RegistryKey>',
'')
    if (havex64)
        cat(file = con, sep="\n",
'        <RegistryKey Root="HKMU" Key="Software\\R-core\\R64">',
'         <RegistryValue Name="InstallPath" Type="string" Value="[INSTALLDIR]"/>',
'         <RegistryValue Name="Current Version" Type="string" Value="[RVersion]"/>',
'        </RegistryKey>',
'        <RegistryKey Root="HKMU" Key="Software\\R-core\\R64\\[RVersion]">',
'         <RegistryValue Name="InstallPath" Type="string" Value="[INSTALLDIR]"/>',
'        </RegistryKey>',
'')

    cat(file = con, sep="\n",
'      </Component>',
'')

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

    if (havex64) {
    cat(file = con, sep="\n",
        '',
        '    <Feature Id="x64" Title="64-bit Files" Description="64-bit binary files" Level="1"',
        '     Absent="disallow"',
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
        "        <ComponentRef Id='shortcut64' />",
        "        <ComponentRef Id='shortcut1' />",
        "      </Feature>",
        "      <Feature Id='dshortcut' Title='Desktop Shortcut' Description='Install Desktop shortcut' Level='1'",
        "       InstallDefault='local' AllowAdvertise='no'>",
        "        <ComponentRef Id='desktopshortcut64' />",
        "      </Feature>",
        '      <Feature Id="qshortcut" Title="Quicklaunch Shortcut" Description="Install Quick Launch shortcut" Level="1000"',
        '       InstallDefault="local" AllowAdvertise="no">',
        "        <ComponentRef Id='quickshortcut0' />",
        "      </Feature>",
        "    </Feature>")

    cat(file = con, sep="\n",
        '    <Feature Id="registryversion" Title="Save Version in Registry"',
        '     Description="Save the R version and install path in the Registry" Level="1" InstallDefault="local" AllowAdvertise="no">',
        "      <ComponentRef Id='registry64' />",
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
