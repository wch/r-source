
[Types]
Name: "user"; Description: {cm:user}; Check: Is64BitInstallMode
Name: "user32"; Description: {cm:user32}
Name: "user64"; Description: {cm:user64}; Check: Is64BitInstallMode
Name: "compact"; Description: {cm:compact32}
Name: "full"; Description: {cm:full}
Name: "add64"; Description: {cm:add64}; Check: Is64BitInstallMode
Name: "custom"; Description: {cm:custom}; Flags: iscustom

[Components]
Name: "main"; Description: "Main Files"; Types: user user32 user64 compact full custom;
Name: "i386"; Description: "i386 Files"; Types: user user32 compact full custom;
Name: "x64"; Description: "x64 Files"; Types: user user64 add64 full custom; Check: Is64BitInstallMode
Name: "html"; Description: "HTML Manuals"; Types: user user32 user64 full custom; Flags:
Name: "manuals"; Description: "On-line PDF Manuals"; Types: user user32 user64 full custom
Name: "manuals/basic"; Description: "Basic Manuals"; Types: user user32 user64 full custom
Name: "manuals/technical"; Description: "Technical Manuals"; Types: full custom
Name: "manuals/refman"; Description: "PDF help pages (reference manual)"; Types: full custom
Name: "manuals/libdocs"; Description: "Docs for Packages grid and Matrix"; Types: full custom
Name: "tcl"; Description: "SupportFiles for Package tcltk"; Types: user full custom; Flags: checkablealone
Name: "tcl/noarch"; Description: "Main Files"; Types: user user32 user64 full custom
Name: "tcl/32"; Description: "i386 Files for Package tcltk"; Types: user user32 full custom
Name: "tcl/64"; Description: "x64 Files for Package tcltk"; Types: user user64 add64 full custom; Check: Is64BitInstallMode
Name: "tcl/tzdata"; Description: "Timezone files for Tcl"; Types: full custom
Name: "tcl/chm"; Description: "Tcl/Tk Help (Compiled HTML)"; Types: full custom
Name: "trans"; Description: "Message Translations"; Types: user user32 user64 full custom
Name: "tests"; Description: "Test files"; Types: full custom
