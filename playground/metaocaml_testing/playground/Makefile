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
	rm -f *.cm[ixo] *.[oa] *~ a.out playground.out

playground: playground.ml square.cmx
	$(MOCAMLOPT) -o $@.out square.cmx playground.ml
	./$@.out
	rm -f playground.out

square.cmo: square.ml
square.cmx: square.ml
