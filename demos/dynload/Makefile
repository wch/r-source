# Set this so that R is found

R = ../../bin/R

all: zero.so

# R SHLIB could also do it in one step ...

zero.so: zero.o
	$(R) SHLIB -o $@ $<

zero.o:	zero.c
	$(R) COMPILE $<

clean:
	@rm -f *.o *.so
distclean: clean
