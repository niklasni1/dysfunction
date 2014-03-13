all:
	ocamlbuild main.native && cp main.native straight-line && ./straight-line < example.sl

clean:
	rm -rf _build
