.PHONY: celan

TOPFILE = rc
BENCH_FILE = bench.exe
OCAMLC = ocamlfind c
OCAMLOPT = ocamlfind opt
OCAMLDEP = ocamlfind dep
SOURCES_HEAD = MenhirLexemes.ml Language.ml
SOURCES_GENERATED =  LamaMenhir.ml LamaLexer.ml
SOURCES_TAIL = RunMenhir.ml SM.ml X86.ml
SOURCES = $(SOURCES_HEAD) $(SOURCES_GENERATED) $(SOURCES_TAIL)
COMPILE_OBJS_CMO := $(SOURCES_HEAD:.ml=.cmo) LamaMenhir.cmo LamaLexer.cmo $(SOURCES_TAIL:.ml=.cmo)
COMPILE_OBJS_CMX := $(SOURCES:.ml=.cmx)  LamaMenhir.cmx LamaLexer.cmx $(SOURCES_TAIL:.ml=.cmx)
LIBS =
OCAMLFIND_PACKAGES=-package GT.syntax.all,ostap.syntax
CAMLP5 = -syntax camlp5o $(OCAMLFIND_PACKAGES)
#PXFLAGS = $(CAMLP5)
BFLAGS = -rectypes -package GT,re,ostap,benchmark,angstrom,opal -linkpkg -w -13 -g
OFLAGS = $(BFLAGS)
MENHIR_FLAGS = --external-tokens MenhirLexemes --explain
#MENHIR_FLAGS += --trace

all: LamaMenhir.ml LamaLexer.ml depend $(TOPFILE).opt $(BENCH_FILE)

#depend: $(SOURCES)
#	$(OCAMLDEP) $(PXFLAGS) *.ml *.mli > .depend

$(TOPFILE).opt: $(COMPILE_OBJS_CMX) Driver.cmx
	$(OCAMLOPT) -o $@ $(OFLAGS) $(LIBS:.cma=.cmxa) $(SOURCES:.ml=.cmx) Driver.cmx

$(BENCH_FILE): $(COMPILE_OBJS_CMX) GenParser.cmx LamaAngstrom.cmx LamaOpal.cmx bench.cmx
	$(OCAMLOPT) -o $@ $(BFLAGS) $(OCAMLFIND_PACKAGES) -package str $(OFLAGS) $^


celan: clean
clean:
	$(RM) -R *.cmi *.cmo *.cmx *.annot *.o *.opt *.byte *~ .depend LamaMenhir.ml LamaMenhir.mli LamaLexer.ml

-include .depend

%.ml: %.mll
	ocamllex $<

SM.cmx Language.cmx: PXFLAGS += $(CAMLP5)
#LamaMenhir.cmo: LamaMenhir.ml LamaMenhir.cmi Language.cmi Language.cmo
LamaMenhir.cmx: LamaMenhir.ml LamaMenhir.cmi Language.cmi Language.cmx MenhirLexemes.cmi MenhirLexemes.cmo
RunMenhir.cmx LamaLexer.cmx: LamaMenhir.cmx Language.cmi
LamaOpal.cmx LamaAngstrom.cmx: Language.cmi GenParser.cmi
X86.cmx SM.cmx: Language.cmi
Driver.cmx: LamaMenhir.cmx LamaLexer.cmx
bench.cmx: LamaOpal.cmx

LamaMenhir.ml: LamaMenhir.mly
	menhir $(MENHIR_FLAGS) $<
	$(RM) LamaMenhir.mli

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
