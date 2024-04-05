clean:
	rm -f *.cm[ixo] *.[oa] *~ a.out

map_benchmark: map.ml
	ocamlfind opt -thread -package core_unix.command_unix,core_bench -linkpkg -o $@.out map.ml
	./$@.out -quota 0.5
