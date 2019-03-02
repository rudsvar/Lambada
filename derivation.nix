{ stdenv, lib, ghc }:

stdenv.mkDerivation rec {
  version = "0.0.1";
  name = "lambada-${version}";
  src = lib.cleanSource ./.;
  nativeBuildInputs = [ ghc ];
  buildPhase = "ghc app/Main.hs -isrc -odir build -o lambada";
  installPhase = "mkdir -p $out/bin && cp lambada $out/bin";
}
