.PHONY: celan

TOPFILE = rc
OCAMLC = ocamlfind c
OCAMLOPT = ocamlfind opt
OCAMLDEP = ocamlfind dep
SOURCES_HEAD = Language.ml
SOURCES_GENERATED =  LamaMenhir.ml LamaLexer.ml
SOURCES_TAIL = SM.ml X86.ml Driver.ml
SOURCES = $(SOURCES_HEAD) $(SOURCES_GENERATED) $(SOURCES_TAIL)
COMPILE_OBJS_CMO := $(SOURCES_HEAD:.ml=.cmo) LamaMenhir.cmo LamaLexer.cmo $(SOURCES_TAIL:.ml=.cmo)
COMPILE_OBJS_CMX := $(SOURCES:.ml=.cmx)  LamaMenhir.cmx LamaLexer.cmx $(SOURCES_TAIL:.ml=.cmx)
#COMPILE_OBJS_CMO := $(COMPILE_OBJS_CMO:.mli=.cmi)
#COMPILE_OBJS_CMX := $(COMPILE_OBJS_CMX:.mli=.cmi)
LIBS =
CAMLP5 = -syntax camlp5o -package GT.syntax.all,ostap.syntax
PXFLAGS = $(CAMLP5)
BFLAGS = -rectypes -package GT,re,ostap -linkpkg -w -13
OFLAGS = $(BFLAGS)

all: LamaMenhir.ml LamaLexer.ml depend $(TOPFILE).opt

depend: $(SOURCES)
	$(OCAMLDEP) $(PXFLAGS) *.ml *.mli > .depend

$(TOPFILE).opt: $(COMPILE_OBJS_CMX)
	$(OCAMLOPT) -o $(TOPFILE).opt $(OFLAGS) $(LIBS:.cma=.cmxa) $(SOURCES:.ml=.cmx)

$(TOPFILE).byte: $(COMPILE_OBJS_CMO)
	$(OCAMLC) -o $(TOPFILE).byte $(BFLAGS) $(LIBS) $(SOURCES:.ml=.cmo)

celan: clean
clean:
	$(RM) -R *.cmi *.cmo *.cmx *.annot *.o *.opt *.byte *~ .depend LamaMenhir.ml LamaMenhir.mli LamaLexer.ml

-include .depend

%.ml: %.mll
	ocamllex $<

LamaMenhir.cmo: LamaMenhir.ml LamaMenhir.cmi Language.cmi Language.cmo
LamaMenhir.cmx: LamaMenhir.ml LamaMenhir.cmi Language.cmi Language.cmx
Driver.cmx: LamaMenhir.cmx LamaLexer.cmx

LamaMenhir.ml: LamaMenhir.mly #LamaMenhir.cmi
	menhir --external-tokens Language.Json --explain  $<

# generic rules

###############
%.cmi: %.mli
	$(OCAMLC) -c $(BFLAGS) $(PXFLAGS) $<

# Note: cmi <- mli should go first
%.cmi: %.ml
	$(OCAMLC) -c $(BFLAGS) $(PXFLAGS) $<

%.cmo: %.ml
	$(OCAMLC) -c $(BFLAGS) $(PXFLAGS) $<

%.o: %.ml
	$(OCAMLOPT) -c $(OFLAGS) $(STATIC) $(PXFLAGS) $<

%.cmx: %.ml
	$(OCAMLOPT) -c $(OFLAGS) $(STATIC) $(PXFLAGS) $<

-include `ocamlc -where`/Makefile.config
