LEX_ML = ocamllex
YACC_ML = ocamlyacc
OCAMLC = ocamlc


aps.ml: aps_ml.mll aps_ml.mly
	$(OCAMLC) -c ast.ml
	$(LEX_ML) -o aps_lex.ml aps_ml.mll
	$(YACC_ML) -b aps_yacc aps_ml.mly
	$(OCAMLC) -c aps_yacc.mli
	$(OCAMLC) -c aps_lex.ml
	$(OCAMLC) -c aps_yacc.ml
	$(OCAMLC) aps_lex.cmo aps_yacc.cmo aps_ml.ml

clean:
	rm -f *.cmo
	rm -f *.cmi
	rm -f aps.ml
	rm -f aps_yacc
	rm -f aps_lex.ml
	rm -f aps_yacc.ml
	rm -f aps_yacc.mli

mrproper: clean
	rm -f a.out
