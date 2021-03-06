EXE = lambada
GHCFLAGS = -Wall -Wextra -j4
BUILDFLAGS = -i$(SRCDIR) -outputdir $(BUILDDIR)
BUILDDIR = build
SRCDIR = src
MAIN = app/Main

build: $(EXE)

$(EXE): FORCE
	ghc $(GHCFLAGS) $(MAIN) $(BUILDFLAGS) -o $@

check: FORCE
	ghc -fno-code $(GHCFLAGS) $(MAIN) $(BUILDFLAGS)

ghci: FORCE
	ghci app/Main.hs -isrc

tags: FORCE
	ghci app/Main.hs -isrc -e ":ctags"

release: FORCE
	ghc $(GHCFLAGS) -O2 $(MAIN) $(BUILDFLAGS) -o $(EXE)

test: FORCE
	ghc $(GHCFLAGS) -itest test/Spec.hs $(BUILDFLAGS) -o $(BUILDDIR)/test
	$(BUILDDIR)/test

clean:
	rm -rf $(EXE) $(BUILDDIR)

docs: FORCE
	stack haddock --no-haddock-deps
	rm -r docs
	cp -r .stack-work/install/x86_64-linux-nix/lts-11.13/8.2.2/doc/lambada-0.1.0.0 docs

.PHONY: FORCE clean
