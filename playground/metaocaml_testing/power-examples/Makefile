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
	rm -f *.cm[ixo] *.[oa] *~ a.out power_rts.out powerf_rts.out pw7.ml alt_power_rts.out

# All tests
tests: power_rts powerf_rts

# Power example
power_rts: power_rts.ml square.cmx
	$(MOCAMLOPT) -o $@.out square.cmx power_rts.ml
	./$@.out
	rm -f power_rts.out

powerf_rts: powerf_rts.cmo
	$(OCAMLC) -o $@.out $@.cmo
	./powerf_rts.out

square.cmo: square.ml
square.cmx: square.ml

alt_power_rts: alt_power_rts.cmo
	$(OCAMLC) -o $@.out $@.cmo
	./alt_power_rts.out