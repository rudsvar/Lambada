{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  name = "ghci";
  nativeBuildInputs = [ pkgs.ghc ];
  shellHook = ''
    ghci -Wall -Wextra src/Main.hs -isrc
    exit
  '';
}
