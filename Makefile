.PHONY: all
all: ini.native ini.byte parcoomTest.native parcoomTest.byte

ini.native: src/parcoom.ml examples/ini.ml
	ocamlfind ocamlopt -I src/ -I examples/ -o ini.native src/parcoom.ml examples/ini.ml

ini.byte: src/parcoom.ml examples/ini.ml
	ocamlfind ocamlc -I src/ -I examples/ -o ini.byte src/parcoom.ml examples/ini.ml

parcoomTest.native: src/parcoom.ml tests/parcoomTest.ml
	ocamlfind ocamlopt -I src/ -I tests/ -o parcoomTest.native src/parcoom.ml tests/parcoomTest.ml

parcoomTest.byte: src/parcoom.ml tests/parcoomTest.ml
	ocamlfind ocamlc -I src/ -I tests/ -o parcoomTest.byte src/parcoom.ml tests/parcoomTest.ml

.PHONY: test
test: test.native test.byte

.PHONY: test.native
test.native: parcoomTest.native
	./parcoomTest.native

.PHONY: test.byte
test.byte: parcoomTest.byte
	./parcoomTest.byte
