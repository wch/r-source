HIMGS =cam.h color.h console.h copy.h copy2.h copypaste.h cut.h erase.h  \
       help.h open.h open2.h paste.h print.h save.h

all: ../stdimg.c ../stdimg.h

../stdimg.c : $(HIMGS)
	echo "#include \"graphapp.h\"" > ../stdimg.c
	cat $(HIMGS) | sed "s/^rgb/static rgb/" | sed "s/^byte/static byte/" |sed "s/^imagedata/static imagedata/" >> ../stdimg.c

../stdimg.h: ../stdimg.c
	grep ^image.*= ../stdimg.c | sed "s/ *=.*/;/" | sed "s/^image/extern image/" > ../stdimg.h


%.h : %.gif imgcnv.exe
	imgcnv $*.gif $*.h

# Commented out to avoid circular dependency
#%.gif : %.h imgcnv.exe
#	imgcnv $*.h $*.gif

#I don't think I will rebuild this too much times, so I don't try
#to be clever
imgcnv.exe: imgcnv.c
	gcc -mwindows -s -o imgcnv imgcnv.c ../*.c -lcomctl32

display.exe: display.c
	gcc -mwindows -s -o display display.c ../*.c -lcomctl32


clean:
	rm -f *~ \#*\# $(HIMGS) imgcnv.exe display.exe

