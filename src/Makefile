OCAMLBUILD = corebuild
CFLAGS = -safe-string
BUILDFLAGS = -use-ocamlfind -cflags $(CFLAGS)

.PHONY: nhc
nhc: update-version
	$(OCAMLBUILD) $(BUILDFLAGS) nhc.native

.PHONY: update-version
update-version:
	./update_version.sh

.PHONY: test
test: nhc
	$(OCAMLBUILD) $(BUILDFLAGS) tester.native
	./tester.native ../test

.PHONY: clean
clean:
	$(OCAMLBUILD) -clean
	rm -f version.ml
