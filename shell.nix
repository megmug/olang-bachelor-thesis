{ pkgs ? import ( builtins.fetchGit {
    # nixos-21.11 at 2022-05-10
    url = "https://github.com/nixos/nixpkgs/";
    ref = "release-21.11";
    rev = "38a25ca6dd1aa84f356d5d7c4d5ba617cc43844a";
  } ) {} }:
pkgs.mkShell {
    buildInputs = [
        	pkgs.ghc
	        pkgs.haskellPackages.haskell-language-server
	        pkgs.stack
    ];
}