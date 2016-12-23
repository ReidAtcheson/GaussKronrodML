


test : test.ml
	ocamlbuild test.native



.PHONY : clean



clean : 
	ocamlbuild -clean
