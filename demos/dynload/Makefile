# Set this so that R is found

R = ../../bin/R

all: zero.so

zero.so: zero.c
	$(R) SHLIB -o $@ zero.c

clean:
	@rm -f *.o *.so
distclean: clean
