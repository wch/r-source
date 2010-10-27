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

.make_R.wxs <- function(RW, srcdir, personal = "0")
{
    have64bit <- file_test("-d", file.path(srcdir, "bin", "x64"))
    have32bit <- file_test("-d", file.path(srcdir, "bin", "i386"))

    personal <- personal == "1"
    ## need DOS-style paths
    srcdir0 <- srcdir
    srcdir <- gsub("/", "\\", srcdir, fixed = TRUE)
    sRW <- shortPathName(srcdir)

    Rver <- readLines("../../../VERSION")[1L]
    Rver <- sub("Under .*$", "Pre-release", Rver)
    SVN <- sub("Revision: ", "", readLines("../../../SVN-REVISION"))[1L]
    Rver0 <- paste(sub(" .*$", "", Rver), SVN, sep = ".")

    uuids <- readLines("uuids")
    nc <- 1
    guuids <- function() {x <- uuids[nc]; nc <<- nc + 1L; x}

    ## for x64 add InstallerVersion="200" Platforms="x64"
    ## see http://blogs.msdn.com/astebner/archive/2007/08/09/4317654.aspx
    ## Or something like
    ##<Condition Message='This application is for x64 Windows.'>
    ##  VersionNT64
    ##</Condition>

    con <- file("R.wxs", "w")
    ## ALLUSERS = 1 for per-machine, blank for default.
    ## http://wix.mindcapers.com/wiki/Allusers_Install_vs._Per_User_Install
    ## For non-elevation  see
    ## http://blogs.msdn.com/astebner/archive/2007/11/18/6385121.aspx
    cat(file = con, sep = "\n",
        '<?xml version="1.0" encoding="windows-1252"?>',
        '<Wix xmlns="http://schemas.microsoft.com/wix/2003/01/wi">',
        '  <Product Manufacturer="R Development Core Team" ',
        '   Id="3AF9DA8E-B4DB-49B2-802D-A279667F44E5"',
        '   Language="1033"',
        sprintf('   Name="R%s %s for Windows (via msi)"',
                ifelse(have64bit, " x64", ""),
                Rver),
        sprintf('   Version="%s"', Rver),
        '   UpgradeCode="309E663C-CA7A-40B9-8822-5D466F1E2AF9">',
        '    <Package Id="????????-????-????-????-????????????" ',
        sprintf('     Keywords="R %s for Windows Installer"', Rver),
        sprintf('     Description="R %s for Windows Installer"', Rver),
        '     Comments="R Language and Environment"',
        '     Manufacturer="R Development Core Team"',
        if (have64bit) '     InstallerVersion="200"'
        else '     InstallerVersion="100"',
        if (have64bit) '     Platforms="x64"',
        '     Languages="1033"',
        '     Compressed="yes"',
        if (personal) 'InstallPrivileges="limited"',
        '     SummaryCodepage="1252" />',
        '    <Media Id=\'1\' Cabinet=\'Sample.cab\' EmbedCab=\'yes\' DiskPrompt="CD-ROM #1" />',
        '   <Property Id=\'DiskPrompt\' Value="R for Windows Installation [1]" />',
        if(personal)'   <Property Id="ALLUSERS"></Property>'
        else '   <Property Id="ALLUSERS">1</Property>',
        '')

    if (have64bit) {
        cat(file = con, sep = "\n", "",
            "<Condition Message='This application is for x64 Windows.'>",
            "  VersionNT64", "</Condition>")
    }

    ## The standard folder names are listed at
    ## http://msdn.microsoft.com/en-us/library/aa372057.aspx
    cat(file = con, sep = "\n", '',
        '    <Directory Id=\'TARGETDIR\' Name=\'SourceDir\'>',
        '',
        if (have64bit)
        '      <Directory Id=\'ProgramFiles64Folder\' Name=\'PFiles\'>'
        else
        '      <Directory Id=\'ProgramFilesFolder\' Name=\'PFiles\'>',
        '        <Directory Id=\'R\' Name=\'R\'>',
        sprintf("          <Directory Id='INSTALLDIR' Name = '%s' LongName='%s'>", sRW, RW))


    ff <- readLines('files.wxs', warn = FALSE)
    ff <- grep("^        ", ff, value = TRUE)
    rx1 <- ' *<Component Id="([^"]*)".*'
    rx2 <- ' *<File Id=\"([^\"]*).* (src|Source)=\"([^\"]*)\".*'
    rx3 <- paste(".*\\", srcdir, "\\", sep = "")
    comps <- ids <- nm <- character()
    rgui <- rgui64 <- rhelp <- 'unknown'
    comp <- id <- 'unknown'
    rx3 <- paste(".*", srcdir0, "/", sep="")
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
            ## These manuals are on the Rgui menu, so should always be installed
            if (g %in%  c("doc/manual/R-FAQ.html",
                          "doc/html/rw-FAQ.html",
                          "share/texmf/Sweave.sty"))
                component <- "main"
            else if (grepl("^doc/html", g) || grepl("^library/[^/]*/html", g) ||
                     g == "library/R.css")
                component <- "main"
            else if (grepl("^doc/manual/[^/]*\\.html", g))
                component <- "html"
            else if (g %in% c("doc/manual/R-data.pdf", "doc/manual/R-intro.pdf"))
                component <- "manuals/basic"
            else if (g %in% c("doc/manual/R-admin.pdf",
                              "doc/manual/R-exts.pdf",
                              "doc/manual/R-ints.pdf",
                              "doc/manual/R-lang.pdf"))
                component <- "manuals/technical"
            else if (g == "doc/manual/refman.pdf")
                component <- "manuals/refman"
            else if (grepl("^doc/manual", g) && g != "doc/manual/R-FAQ.pdf")
                component <- "manuals"
            else if (grepl("^library/[^/]*/tests", g) || grepl("^tests", g))
	    	component <- "tests"
            else if (have64bit && grepl("^Tcl/(bin|lib)64", g))
                component <- "tcl/64"
            else if (have64bit &&
                     (grepl("^Tcl/bin", g) ||
                      grepl("^Tcl/lib/(dde1.3|reg1.2|Tktable)", g)))
                component <- "tcl/32"
            else if (grepl("^Tcl/doc/.*chm$", g))
                component <- "tcl/chm"
            else if (grepl("^Tcl/lib/tcl8.5/tzdata", g))
                component <- "tcl/tzdata"
            else if (grepl("^Tcl", g))
                component <- "tcl/noarch"
            else if (grepl("^library/grid/doc", g) ||
                     grepl("^library/Matrix/doc", g))
                component <- "manuals/libdocs"
            else if (grepl("^share/locale", g) ||
                     grepl("^library/[^/]*/po", g))
                component <- "trans"
            else if (have64bit && grepl("/i386/", g)) component <- "i386"
            else if (have64bit && grepl("/x64/", g)) component <- "x64"
            else
                component <- "main"
            comps <- c(comps, component)
        }
        if(grepl("PUT-GUID-HERE", f)) {
            f <- sub("PUT-GUID-HERE", uuids[nc], f)
            nc <- nc + 1L
        }
        cat("    ", f, "\n", file=con, sep="")
    }
    if (rgui == "unknown") rgui <- rgui64

    cat(file = con, sep="\n",
        "          </Directory>",
        "        </Directory>",
        "      </Directory>")

    cat(file = con, sep="\n",
'      <Directory Id="StartMenuFolder" Name="SMenu">',
'        <Directory Id="ProgramMenuFolder" Name="Programs">',
'          <Directory Id="RMENU" Name="R">')
    if (have32bit)
        cat(file = con, sep="\n",
'            <Component Id="shortcut0"',
sprintf('             Guid="%s" KeyPath="yes">', guuids()),
'              <Shortcut Id="RguiStartMenuShortcut" Directory="RMENU" Name="R" ',
sprintf('               LongName="R %s" Target="[!%s]" ', Rver, rgui),
'               WorkingDirectory="INSTALLDIR" />',
'            </Component>')
    if (have64bit)
        cat(file = con, sep="\n",
'            <Component Id="shortcut64"',
sprintf('             Guid="%s" KeyPath="yes">', guuids()),
'              <Shortcut Id="Rgui64StartMenuShortcut" Directory="RMENU" Name="R64" ',
sprintf('               LongName="R x64 %s" Target="[!%s]" ', Rver, rgui64),
'               WorkingDirectory="INSTALLDIR" />',
'            </Component>')

    cat(file = con, sep="\n",
'            <Component Id="shortcut1" ',
sprintf('             Guid="%s" KeyPath="yes">', guuids()),
'              <Shortcut Id="HelpStartMenuShortcut" Directory="RMENU" ',
sprintf('               Name="RHelp" LongName="R %s Help" Target="[!%s]"', Rver, rhelp),
'               WorkingDirectory="INSTALLDIR" />',
'            </Component>',
'          </Directory>',
'        </Directory>',
'      </Directory>',
'      <Directory Id="DesktopFolder" Name="Desktop">')
    if (have32bit)
        cat(file = con, sep="\n",
sprintf('        <Component Id="desktopshortcut0" DiskId="1" Guid="%s">', guuids()),
sprintf('          <Shortcut Id="RguiDesktopShortcut" Directory="DesktopFolder" Name="R" LongName="R %s"', Rver),
sprintf('           WorkingDirectory="INSTALLDIR" Target="[!%s]" />', rgui),
'        </Component>')
    if (have64bit)
        cat(file = con, sep="\n",
sprintf('        <Component Id="desktopshortcut64" DiskId="1" Guid="%s">', guuids()),
sprintf('          <Shortcut Id="Rgui64DesktopShortcut" Directory="DesktopFolder" Name="Rx64" LongName="R x64 %s"', Rver),
sprintf('           WorkingDirectory="INSTALLDIR" Target="[!%s]" />', rgui64),
'        </Component>')
    cat(file = con, sep="\n", '      </Directory>',
'',
'      <Directory Id="AppDataFolder" Name="AppData">',
'        <Directory Id="Microsoft" Name="MS" LongName="Microsoft">',
'          <Directory Id="InternetExplorer" Name="IE" LongName="Internet Explorer">',
'            <Directory Id="QuickLaunch" Name="QLaunch" LongName="Quick Launch">',
sprintf('              <Component Id="quickshortcut0" DiskId="1" Guid="%s">', guuids()),
sprintf('                <Shortcut Id="RguiQuickShortcut" Directory="QuickLaunch" Name="R" LongName="R %s"', Rver),
sprintf('                 WorkingDirectory="INSTALLDIR" Target="[!%s]" />', rgui),
'              </Component>',
'            </Directory>',
'          </Directory>',
'        </Directory>',
'      </Directory>',
'',
'')
if (have32bit) { # go in 32-bit registry
    cat(file = con, sep="\n",
sprintf('      <Component Id="registry0" Guid="%s">', guuids()),
'        <Registry Id="RInstallPath" Root="HKMU" Key="Software\\R-core\\R" ',
'         Name="InstallPath" Type="string" KeyPath="yes" Value="[INSTALLDIR]" />',
'      </Component>',
sprintf('      <Component Id="registry7" Guid="%s">', guuids()),
'        <Registry Id="RVerInstallPath" Root="HKMU" ',
'         Key="Software\\R-core\\R" Name="InstallPath"',
'         Type="string" KeyPath="yes" Value="[INSTALLDIR]" />',
'      </Component>',
sprintf('      <Component Id="registry1" Guid="%s">', guuids()),
'        <Registry Id="RCurrentVersion" Root="HKMU" Key="Software\\R-core\\R" ',
'         Name="Current Version" Type="string" KeyPath="yes" ',
'         Value="[ProductVersion]" />',
'      </Component>',
sprintf('      <Component Id="registry2" Guid="%s">', guuids()),
'        <Registry Id="RCurrentVerInstallPath" Root="HKMU" ',
'         Key="Software\\R-core\\R\\[ProductVersion]" Name="InstallPath"',
'         Type="string" KeyPath="yes" Value="[INSTALLDIR]" />',
'      </Component>',
sprintf('      <Component Id="registry20" Guid="%s">', guuids()),
'        <Registry Id="R32InstallPath" Root="HKMU" Key="Software\\R-core\\R32" ',
'         Name="InstallPath" Type="string" KeyPath="yes" Value="[INSTALLDIR]" />',
'      </Component>',
sprintf('      <Component Id="registry21" Guid="%s">', guuids()),
'        <Registry Id="R32VerInstallPath" Root="HKMU" ',
'         Key="Software\\R-core\\R32" Name="InstallPath"',
'         Type="string" KeyPath="yes" Value="[INSTALLDIR]" />',
'      </Component>',
sprintf('      <Component Id="registry22" Guid="%s">', guuids()),
'        <Registry Id="R32CurrentVersion" Root="HKMU" Key="Software\\R-core\\R32" ',
'         Name="Current Version" Type="string" KeyPath="yes" ',
'         Value="[ProductVersion]" />',
'      </Component>',
sprintf('      <Component Id="registry23" Guid="%s">', guuids()),
'        <Registry Id="R32CurrentVerInstallPath" Root="HKMU" ',
'         Key="Software\\R-core\\R32\\[ProductVersion]" Name="InstallPath"',
'         Type="string" KeyPath="yes" Value="[INSTALLDIR]" />',
'      </Component>',
'')
}

 if (have64bit) { # go in 64-bit registry
    cat(file = con, sep="\n",
sprintf('      <Component Id="registry60" Guid="%s" Win64="yes">', guuids()),
'        <Registry Id="Rx64InstallPath" Root="HKMU" Key="Software\\R-core\\R" ',
'         Name="InstallPath" Type="string" KeyPath="yes" Value="[INSTALLDIR]" />',
'      </Component>',
sprintf('      <Component Id="registry67" Guid="%s" Win64="yes">', guuids()),
'        <Registry Id="Rx64VerInstallPath" Root="HKMU" ',
'         Key="Software\\R-core\\R" Name="InstallPath"',
'         Type="string" KeyPath="yes" Value="[INSTALLDIR]" />',
'      </Component>',
sprintf('      <Component Id="registry61" Guid="%s" Win64="yes">', guuids()),
'        <Registry Id="Rx64CurrentVersion" Root="HKMU" Key="Software\\R-core\\R" ',
'         Name="Current Version" Type="string" KeyPath="yes" ',
'         Value="[ProductVersion]" />',
'      </Component>',
sprintf('      <Component Id="registry62" Guid="%s" Win64="yes">', guuids()),
'        <Registry Id="Rx64CurrentVerInstallPath" Root="HKMU" ',
'         Key="Software\\R-core\\R\\[ProductVersion]" Name="InstallPath"',
'         Type="string" KeyPath="yes" Value="[INSTALLDIR]" />',
'      </Component>',
 sprintf('      <Component Id="registry30" Guid="%s" Win64="yes">', guuids()),
'        <Registry Id="R64InstallPath" Root="HKMU" Key="Software\\R-core\\R64" ',
'         Name="InstallPath" Type="string" KeyPath="yes" Value="[INSTALLDIR]" />',
'      </Component>',
sprintf('      <Component Id="registry31" Guid="%s" Win64="yes">', guuids()),
'        <Registry Id="R64VerInstallPath" Root="HKMU" ',
'         Key="Software\\R-core\\R64" Name="InstallPath"',
'         Type="string" KeyPath="yes" Value="[INSTALLDIR]" />',
'      </Component>',
sprintf('      <Component Id="registry32" Guid="%s" Win64="yes">', guuids()),
'        <Registry Id="R64CurrentVersion" Root="HKMU" Key="Software\\R-core\\R64" ',
'         Name="Current Version" Type="string" KeyPath="yes" ',
'         Value="[ProductVersion]" />',
'      </Component>',
sprintf('      <Component Id="registry33" Guid="%s" Win64="yes">', guuids()),
'        <Registry Id="R64CurrentVerInstallPath" Root="HKMU" ',
'         Key="Software\\R-core\\R64\\[ProductVersion]" Name="InstallPath"',
'         Type="string" KeyPath="yes" Value="[INSTALLDIR]" />',
'      </Component>')
}

    ## file associations
    cat(file = con, sep="\n",
sprintf('      <Component Id="registry3" Guid="%s">', guuids()),
'        <Registry Id="RData" Root="HKCR" Key=".RData" Type="string"',
'         KeyPath="yes" Value="RWorkspace" />',
'      </Component>',
sprintf('      <Component Id="registry4" Guid="%s">', guuids()),
'        <Registry Id="RWorkspace" Root="HKCR" Key="RWorkspace" Type="string" ',
'         KeyPath="yes" Value="R Workspace" />',
'      </Component>',
sprintf('      <Component Id="registry5" Guid="%s">', guuids()),
'        <Registry Id="RDataCommand" Root="HKCR" ',
'         Key="RWorkspace\\shell\\open\\command" Type="string" KeyPath="yes" ',
sprintf('         Value="&quot;[!%s]&quot; &quot;%%1&quot;" />', rgui),
'      </Component>',
sprintf('      <Component Id="registry6" Guid="%s">', guuids()),
'        <Registry Id="RDataDefaultIcon" Root="HKCR" ',
'         Key="RWorkspace\\DefaultIcon" Type="string" KeyPath="yes" ',
sprintf('         Value="[!%s],0" />', rgui),
'      </Component>',
'    </Directory>')

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

    if (have64bit && have32bit) {
    cat(file = con, sep="\n",
        '',
        '    <Feature Id="i386" Title="i386 Files" Description="32-bit binary files" Level="1"',
        '     ConfigurableDirectory="INSTALLDIR"',
        '     InstallDefault="local" AllowAdvertise="no">')
    for(id in ids[comps == 'i386'])
        cat(file = con,
            "      <ComponentRef Id='", id, "' />\n", sep="")
    cat(file = con, '    </Feature>\n')
    }

    if (have64bit) {
    cat(file = con, sep="\n",
        '',
        '    <Feature Id="x64" Title="x64 Files" Description="64-bit binary files" Level="1"',
        '     ConfigurableDirectory="INSTALLDIR"',
        if (!have32bit) '     Absent="disallow"',
        '     InstallDefault="local" AllowAdvertise="no">')
    for(id in ids[comps == 'x64'])
        cat(file = con,
            "      <ComponentRef Id='", id, "' />\n", sep="")
    cat(file = con, '    </Feature>\n')
    }

    cat(file = con, sep="\n",
        '',
        '    <Feature Id="html" Title="HTML Manuals" Description="HTML versions of the manuals" Level="1"',
        '     InstallDefault="local" AllowAdvertise="no">')
    for(id in ids[comps == 'html'])
        cat(file = con,
            "      <ComponentRef Id='", id, "' />\n", sep="")
    cat(file = con, '    </Feature>\n')


    cat(file = con, sep="\n",
        '',
        '    <Feature Id="manuals" Title="On-line PDF Manuals" Description="On-line PDF Manuals" Level="1"',
        '     InstallDefault="local" AllowAdvertise="no" Display="expand">')

    cat(file = con, sep="\n",
        '',
        '      <Feature Id="manualsb" Title="Basic manuals" Description="Basic Manuals in PDF" Level="1"',
        '       InstallDefault="local" AllowAdvertise="no">')
    for(id in ids[comps == 'manuals/basic'])
        cat(file = con,
            "      <ComponentRef Id='", id, "' />\n", sep="")
    cat(file = con, '      </Feature>\n')

    cat(file = con, sep="\n",
        '',
        '      <Feature Id="manualst" Title="Technical Manuals" Description="Technical Manuals in PDF" Level="1000"',
        '       InstallDefault="local" AllowAdvertise="no">')
    for(id in ids[comps == 'manuals/technical'])
        cat(file = con,
            "      <ComponentRef Id='", id, "' />\n", sep="")
    cat(file = con, '      </Feature>\n')

    cat(file = con, sep="\n",
        '',
        '      <Feature Id="refman" Title="Reference Manual" Description="Reference Manual (help pages in PDF)" Level="1000"',
        '       InstallDefault="local" AllowAdvertise="no">')
    for(id in ids[comps == 'manuals/refman'])
        cat(file = con,
            "      <ComponentRef Id='", id, "' />\n", sep="")
    cat(file = con, '      </Feature>\n')

    cat(file = con, sep="\n",
        '',
        '      <Feature Id="libdocs" Title="Docs for Packages grid and Matrix" Description="Docs for packages grid and Matrix: mainly PDF vignettes and their sources and code" Level="1000"',
        '       InstallDefault="local" AllowAdvertise="no">')
    for(id in ids[comps == 'manuals/libdocs'])
        cat(file = con,
            "      <ComponentRef Id='", id, "' />\n", sep="")
    cat(file = con, '      </Feature>\n')

    cat(file = con, '    </Feature>\n')

    cat(file = con, sep="\n",
        '',
        '    <Feature Id="tcl" Title="Support Files for Package tcltk" Description="A binary distribution of Tcl/Tk for use by R package tcltk" Level="1"',
        '     InstallDefault="local" AllowAdvertise="no" Display="expand">')

    cat(file = con, sep="\n",
        '',
        '      <Feature Id="tcl0" Title="Main files" Description="Main files" Level="1"',
        '       InstallDefault="local" AllowAdvertise="no">')
    for(id in ids[comps == 'tcl/noarch'])
        cat(file = con,
            "      <ComponentRef Id='", id, "' />\n", sep="")
    cat(file = con, '      </Feature>\n')

    if (have64bit && have32bit) {
    cat(file = con, sep="\n",
        '',
        '      <Feature Id="tcl32" Title="i386 Files for Package tcltk" Description="32-bit files for package tcltk" Level="1"',
        '       InstallDefault="local" AllowAdvertise="no">')
    for(id in ids[comps == 'tcl/32'])
        cat(file = con,
            "      <ComponentRef Id='", id, "' />\n", sep="")
    cat(file = con, '      </Feature>\n')
    }
    if (have64bit) {
    cat(file = con, sep="\n",
        '',
        '      <Feature Id="tcl64" Title="x64 Files for Package tcltk" Description="64-bit files for package tcltk" Level="1"',
        '       InstallDefault="local" AllowAdvertise="no">')
    for(id in ids[comps == 'tcl/64'])
        cat(file = con,
            "      <ComponentRef Id='", id, "' />\n", sep="")
    cat(file = con, '      </Feature>\n')
    }

    cat(file = con, sep="\n",
        '',
        '      <Feature Id="tcl1" Title="Tcl/Tk Help (Compiled HTML)" Description="Tcl/Tk Help (Compiled HTML)" Level="1000"',
        '       InstallDefault="local" AllowAdvertise="no">')
    for(id in ids[comps == 'tcl/chm'])
        cat(file = con,
            "      <ComponentRef Id='", id, "' />\n", sep="")
    cat(file = con, '      </Feature>\n')

    cat(file = con, sep="\n",
        '',
        '      <Feature Id="tcl2" Title="Timezone files for Tcl" Description="Timezone files for Tcl" Level="1000"',
        '       InstallDefault="local" AllowAdvertise="no">')
    for(id in ids[comps == 'tcl/tzdata'])
        cat(file = con,
            "      <ComponentRef Id='", id, "' />\n", sep="")
    cat(file = con, '      </Feature>\n')

    cat(file = con, '    </Feature>\n')


    cat(file = con, sep="\n",
        '',
        '    <Feature Id="trans" Title="Message Translations" Description="Messages translated to other languages" Level="1"',
        '     InstallDefault="local" AllowAdvertise="no">')
    for(id in ids[comps == 'trans'])
        cat(file = con,
            "      <ComponentRef Id='", id, "' />\n", sep="")
    cat(file = con, '    </Feature>\n')

     cat(file = con, sep="\n",
        '',
        '    <Feature Id="tests" Title="Test Files" Description="Test files" Level="1000"',
        '     InstallDefault="local" AllowAdvertise="no">')
    for(id in ids[comps == 'tests'])
        cat(file = con,
            "      <ComponentRef Id='", id, "' />\n", sep="")
    cat(file = con, '    </Feature>\n')


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
        "    </Feature>",
        '    <Feature Id="registryversion" Title="Save Version in Registry"',
        '     Description="Save the R version and install path in the Registry" Level="1" InstallDefault="local" AllowAdvertise="no">',
        if (have32bit) "      <ComponentRef Id='registry0' />",
        if (have32bit) "      <ComponentRef Id='registry1' />",
        if (have32bit) "      <ComponentRef Id='registry7' />",
        if (have32bit) "      <ComponentRef Id='registry2' />",
        if (have32bit) "      <ComponentRef Id='registry20' />",
        if (have32bit) "      <ComponentRef Id='registry21' />",
        if (have32bit) "      <ComponentRef Id='registry22' />",
        if (have32bit) "      <ComponentRef Id='registry23' />",
        if (have64bit) "      <ComponentRef Id='registry60' />",
        if (have64bit) "      <ComponentRef Id='registry61' />",
        if (have64bit) "      <ComponentRef Id='registry67' />",
        if (have64bit) "      <ComponentRef Id='registry62' />",
        if (have64bit) "      <ComponentRef Id='registry30' />",
        if (have64bit) "      <ComponentRef Id='registry31' />",
        if (have64bit) "      <ComponentRef Id='registry32' />",
        if (have64bit) "      <ComponentRef Id='registry33' />",
        "    </Feature>",
        '    <Feature Id="associate" Title="Associate with .RData files"',
        '     Description="Associate R with .RData files" Level="1" InstallDefault="local" AllowAdvertise="no">',
        "      <ComponentRef Id='registry3' />",
        "      <ComponentRef Id='registry4' />",
        "      <ComponentRef Id='registry5' />",
        "      <ComponentRef Id='registry6' />",
        "    </Feature>",
        "",
        '    <UIRef Id="WixUI_FeatureTree" />',
        '    <UIRef Id="WixUI_ErrorProgressText" />',
        '',
        '    <Icon Id="shell32.dll" SourceFile="C:\\Windows\\system32\\shell32.dll" />',
        '',
        "  </Product>",
        "</Wix>")

    close(con)
}


args <- commandArgs(TRUE)
do.call(".make_R.wxs", as.list(args))
