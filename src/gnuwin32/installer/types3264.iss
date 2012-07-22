
[Types]
Name: "user"; Description: {cm:user}; Check: Is64BitInstallMode
Name: "user32"; Description: "32-bit {cm:user}"
Name: "user64"; Description: "64-bit {cm:user}"; Check: Is64BitInstallMode
Name: "custom"; Description: {cm:custom}; Flags: iscustom

[Components]
Name: "main"; Description: "Core Files"; Types: user user32 user64  custom
Name: "i386"; Description: "32-bit Files"; Types: user user32 custom
Name: "x64"; Description: "64-bit Files"; Types: user user64 custom
Name: "translations"; Description: "Message translations"; Types: user user64 custom
