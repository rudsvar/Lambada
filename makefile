EXE = lambada
GHCFLAGS = -Wall -Wextra
BUILDFLAGS = -i$(SRCDIR) -odir $(BUILDDIR) -hidir $(BUILDDIR)
BUILDDIR = build
SRCDIR = src
MAIN = app/Main.hs

$(EXE): FORCE
	ghc $(GHCFLAGS) $(MAIN) $(BUILDFLAGS) -o $(EXE)

check: FORCE
	ghc -fno-code $(GHCFLAGS) $(MAIN) $(BUILDFLAGS)

tags: FORCE
	ghci app/Main.hs -isrc -e ":ctags"

release: FORCE
	ghc $(GHCFLAGS) -O2 $(MAIN) $(BUILDFLAGS) -o $(EXE)

clean:
	rm -rf $(EXE) $(BUILDDIR)

docs: FORCE
	stack haddock --no-haddock-deps
	rm -r docs
	cp -r .stack-work/install/x86_64-linux-nix/lts-11.13/8.2.2/doc/lambada-0.1.0.0 docs

.PHONY: FORCE clean
