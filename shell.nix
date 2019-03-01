{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  name = "ghci";
  nativeBuildInputs = [ pkgs.ghc ];
  shellHook = ''
    ghci -Wall -Wextra app/Main.hs -isrc
    exit
  '';
}
