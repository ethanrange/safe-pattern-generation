# Makefile for the MetaOCaml tutorial

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
	rm -f *.cm[ixo] *.[oa] *~ a.out unrolling.out

unrolling: unrolling.ml
	$(MOCAMLOPT) -o $@.out unrolling.ml
	./$@
	rm -f unrolling.out
