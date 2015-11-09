OCAMLBUILD = corebuild
OCAMLFLAGS = -use-ocamlfind

.PHONY: nhc
nhc: update-version
	$(OCAMLBUILD) $(OCAMLFLAGS) nhc.native

.PHONY: update-version
update-version:
	./update_version.sh

.PHONY: clean
clean:
	$(OCAMLBUILD) -clean
	rm -f version.ml
