with import <nixpkgs> {};

stdenv.mkDerivation rec {
  version = "0.0.1";
  name = "lambada-${version}";
  src = ./.;
  nativeBuildInputs = [ pkgs.ghc ];
  buildPhase = "ghc src/Main.hs -isrc -odir build -o lambada";
  installPhase = "mkdir -p $out/bin && cp lambada $out/bin";
}
