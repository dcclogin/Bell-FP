# The paper: bell-fp.pdf, built from bell-fp.tex.
#
#   make            refresh the ASCII copies of the quoted sources, rerun the
#                   data program if the code changed, then build the PDF
#   make sources    the ASCII copies and the listings language only (src-ascii/)
#   make generated  rerun the data program unconditionally
#
# The paper quotes the package (../src/CHSH) and its own examples
# (gen/Examples.hs) by line number, through the copies that asciify.py
# writes, without their comments; asciify.py stops if a quoted range takes
# in a line that is not code. Its numbers, tables and printed reports come
# from generated/, which the program in gen/ writes by running the package;
# it stops if a claim the paper makes about them does not hold. Needs
# Python 3, GHC 9.4 with cabal, and tectonic; the code is set in the system
# font Ioskeley Mono, loaded by name.

TECTONIC ?= tectonic
GEN      := cd gen && cabal run -v0 bell-fp-paper-data -- ../generated
CODE     := $(wildcard ../src/CHSH/*.hs) $(wildcard gen/*.hs) gen/bell-fp-paper.cabal
# the program writes every file in generated/; the values file stands for them
DATA     := generated/values.tex
PDF      := bell-fp.pdf

.PHONY: all sources generated clean

all: $(PDF)

sources:
	python3 asciify.py

generated:
	$(GEN)

$(DATA): $(CODE)
	$(GEN)

$(PDF): sources $(DATA) bell-fp.tex references.bib
	$(TECTONIC) -X compile --keep-logs bell-fp.tex

clean:
	rm -f bell-fp.log bell-fp.blg
