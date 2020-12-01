{ pkgs ? import <nixpkgs> { } }:
let
  project = pkgs.haskellPackages.callPackage ./project.nix { };
in pkgs.mkShell {
  buildInputs = [
    pkgs.haskellPackages.hlint
    pkgs.haskellPackages.haskell-language-server
    pkgs.haskellPackages.cabal-install
    pkgs.haskellPackages.cabal-fmt
    pkgs.haskellPackages.cabal2nix
    ];
  inputsFrom = [ project.env ];
}
