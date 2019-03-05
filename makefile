EXE = lambada
GHCFLAGS = -Wall -Wextra
BUILDFLAGS = -i$(SRCDIR) -odir $(BUILDDIR) -hidir $(BUILDDIR)
BUILDDIR = build
SRCDIR = src
MAIN = app/Main.hs

$(EXE): FORCE
	ghc $(GHCFLAGS) $(MAIN) $(BUILDFLAGS) -o $@

clean:
	rm -rf $(EXE) $(BUILDDIR)

docs: FORCE
	stack haddock --no-haddock-deps
	rm -r docs
	cp -r .stack-work/install/x86_64-linux-nix/lts-11.13/8.2.2/doc/lambada-0.1.0.0 docs

.PHONY: FORCE clean
