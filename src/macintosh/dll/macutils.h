OSErr GetDirectoryID(short vRefNum, long, StringPtr, long *, Boolean *);
OSErr FSpGetDirectoryID(const FSSpec *, long *, Boolean *);
OSErr FSMakeFSSpecFromPath(ConstStr255Param, FSSpecPtr);
