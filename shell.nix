{ pkgs ? import <nixpkgs> { } }:
let
  project = pkgs.haskellPackages.callPackage ./project.nix { };
in pkgs.mkShell {
  buildInputs = with pkgs.haskellPackages; [
    hlint
    haskell-language-server
    cabal-install
    cabal-fmt
    cabal2nix
    ];
  inputsFrom = [ project.env ];
}
