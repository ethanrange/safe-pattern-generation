OCAMLC=metaocamlc
OCAMLOPT=ocamlopt
MOCAMLOPT=metaocamlopt
MOCAML=metaocaml

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(OCAMLC) -c $<
.ml.cmo:
	$(OCAMLC) -c $<
.ml.cmx:
	$(OCAMLOPT) -c $<

clean:
	rm -f *.cm[ixo] *.[oa] *~ a.out unrolling.out benchmark.out

generators: generators.ml
	$(MOCAMLOPT) -o $@.out generators.ml
	./$@.out
	rm -f $@.out

benchmark: benchmark.ml
	ocamlfind opt -thread -package core_unix.command_unix,core_bench -linkpkg -o $@.out benchmark.ml
	./$@.out -quota 1.0
