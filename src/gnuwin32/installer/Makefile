# Makefile for R for Windows Installer

PREFIX=rw0641
include ../MkRules
UNZIPDIR=unzip

.PHONY: GA UZ

all: R.ico GA UZ rwinst.exe
CFLAGS=-I../graphapp -I../../include -I$(UNZIPDIR)/windll -Wall -O1 -DRVER=$(PREFIX)

GA:
	$(MAKE) -C ../graphapp

UZ:
	$(MAKE) -C $(UNZIPDIR) -f ../Makefile.unzip

R.ico: ../front-ends/R.ico
	$(CP) -p ../front-ends/R.ico .

rwico.o: rwico.rc

rwinst.exe: rwinst.o rwico.o dounzip.o ../ga.a $(UNZIPDIR)/unzip32.a
	$(CC) -s -mwindows -o $@ $^ -lcomctl32

clean:
	$(RM) R.ico *.o *~ \#*\#
	$(MAKE) -C $(UNZIPDIR) -f ../Makefile.unzip clean

