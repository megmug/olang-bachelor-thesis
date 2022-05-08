{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/tags/21.11.tar.gz") {} }:

pkgs.mkShell {
    buildInputs = [
        	pkgs.ghc
	        pkgs.haskellPackages.haskell-language-server
	        pkgs.stack
    ];
}