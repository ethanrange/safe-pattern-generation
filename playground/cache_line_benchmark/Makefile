clean:
	rm -f cacheline.out

cacheline: cacheline.c
	cc -O3 -o $@.out cacheline.c
	./$@.out
