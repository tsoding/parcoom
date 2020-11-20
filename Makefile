.PHONY: all
all: ini.native ini.byte

ini.native: src/parcoom.ml examples/ini.ml
	ocamlfind ocamlopt -I src/ -I examples/ -o ini.native src/parcoom.ml examples/ini.ml

ini.byte: src/parcoom.ml examples/ini.ml
	ocamlfind ocamlc -I src/ -I examples/ -o ini.byte src/parcoom.ml examples/ini.ml
