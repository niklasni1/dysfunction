all:
	ocamlbuild main.native && cp main.native straight-line && ./straight-line < example2.sl

clean:
	rm -rf _build
