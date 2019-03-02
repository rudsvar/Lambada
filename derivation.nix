{ stdenv, lib, ghc }:

stdenv.mkDerivation rec {
  version = "0.0.1";
  name = "lambada-${version}";
  src = lib.cleanSource ./.;
  nativeBuildInputs = [ ghc ];
  installPhase = "mkdir -p $out/bin && cp lambada $out/bin";
}
