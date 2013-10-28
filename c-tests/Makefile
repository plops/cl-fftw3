all: sizes

sizes: sizes.c
	gcc -I/opt/local/include sizes.c -L/opt/local/lib -lfftw3 -lm -o sizes

clean:
	rm -f *.o *~ sizes
 
