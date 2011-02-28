
[Types]
Name: "user"; Description: {cm:user}
Name: "compact"; Description: {cm:compact}
Name: "full"; Description: {cm:full}
Name: "custom"; Description: {cm:custom}; Flags: iscustom

[Components]
Name: "main"; Description: "Main Files"; Types: user compact full custom; Flags: fixed
Name: "i386"; Description: "i386 Files"; Types: user compact full custom; Flags: fixed
Name: "html"; Description: "HTML Manuals"; Types: user full custom
Name: "manuals"; Description: "On-line PDF Manuals"; Types: user full custom
Name: "manuals/basic"; Description: "Basic Manuals"; Types: user full custom
Name: "manuals/technical"; Description: "Technical Manuals"; Types: full custom
Name: "manuals/refman"; Description: "PDF help pages (reference manual)"; Types: full custom
Name: "manuals/libdocs"; Description: "Docs for Packages grid, Matrix and survival"; Types: full custom
Name: "tcl"; Description: "Support Files for Package tcltk"; Types: user full custom
Name: "tcl/noarch"; Description: "Main Files"; Types: user full custom
Name: "tcl/tzdata"; Description: "Timezone files for Tcl"; Types: full custom
Name: "tcl/chm"; Description: "Tcl/Tk Help (Compiled HTML)"; Types: full custom
Name: "tcl/msg"; Description: "Message translations for Tcl/Tk"; Types: full custom
Name: "trans"; Description: "Message Translations"; Types: user full custom
Name: "tests"; Description: "Test files"; Types: full custom
