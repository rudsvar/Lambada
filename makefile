EXE = lambada
GHCFLAGS = -Wall -Wextra
BUILDFLAGS = -i$(SRCDIR) -odir $(BUILDDIR) -hidir $(BUILDDIR)
BUILDDIR = build
SRCDIR = src
MAIN = $(SRCDIR)/Main.hs

$(EXE): FORCE
	ghc $(GHCFLAGS) $(MAIN) $(BUILDFLAGS) -o $@

clean:
	rm -rf $(EXE) $(BUILDDIR)

.PHONY: FORCE clean
