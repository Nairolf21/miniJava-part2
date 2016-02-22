all: clean byte

clean:
	ocamlbuild -clean

byte:
	ocamlbuild Main.byte

native:
	ocamlbuild Main.clean

