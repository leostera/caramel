OCAMLC = ocamlc.opt bigarray.cma
OCAMLCG = $(OCAMLC) -g
OCAMLOPT = ocamlopt.opt -g bigarray.cmxa

all:
	make cmis
	make omd

cmis:omd_utils.cmi omd_representation.cmi omd_backend.cmi omd_lexer.cmi omd_lexer_fs.cmi omd_parser.cmi omd.cmi omd_main.cmi

omd:omd_utils.cmx omd_representation.cmx omd_backend.cmx omd_lexer.cmx omd_parser.cmx omd.cmx omd_main.cmx
	$(OCAMLOPT) -o $@ $+

omd.byte:omd_utils.cmo omd_representation.cmo omd_backend.cmo omd_lexer.cmo omd_parser.cmo omd.cmo omd_main.cmo
	$(OCAMLCG) -o $@ $+

omd.opt:omd_utils.cmx omd_representation.cmx omd_backend.cmx omd_lexer.cmx omd_parser.cmx omd.cmx omd_main.cmx
	$(OCAMLOPT) -o $@ $+

omd.cma:omd_utils.cmo omd_representation.cmo omd_backend.cmo omd_lexer.cmo omd_parser.cmo omd.cmo
	$(OCAMLCG) -a -o $@ $+

%.cmo:%.ml %.cmi
	$(OCAMLCG) -c $<

%.cmx:%.ml %.cmi
	$(OCAMLOPT) -c $<

%.cmi:%.mli
	$(OCAMLC) -c $<

clean:
	rm -f *.cmx *.cmo *~ *.cmi *.o *.cma md{,.byte,.opt} omd{,.byte,.opt}

omd.cmi:omd_representation.cmi omd_parser.cmi omd_lexer.cmi omd_backend.cmi
omd_backend.cmi:omd_utils.cmi omd_representation.cmi
omd_lexer.cmi:omd_representation.cmi
omd_lexer_fs.cmi:omd_lexer.cmi
omd_main.cmi:omd_utils.cmi omd_parser.cmi omd_backend.cmi omd.cmi
omd_parser.cmi:omd_utils.cmi omd_representation.cmi omd_lexer.cmi omd_backend.cmi
omd_representation.cmi:omd_utils.cmi

include .depend

.depend:omd_utils.ml omd_representation.ml omd_backend.ml omd_lexer.ml omd_parser.ml omd.ml omd_main.ml omd_utils.mli omd_representation.mli omd_backend.mli omd_lexer.mli omd_parser.mli omd.mli omd_main.mli Makefile
	ocamldep omd_utils.ml omd_representation.ml omd_backend.ml omd_lexer.ml omd_parser.ml omd.ml omd_main.ml omd_utils.mli omd_representation.mli omd_backend.mli omd_lexer.mli omd_parser.mli omd.mli omd_main.mli > .depend


test:
	make clean
	cd .. && make test
	make