{ pkgs ? import ( builtins.fetchGit {
    # nixos-21.11 at 2022-05-09
    url = "https://github.com/nixos/nixpkgs/";
    ref = "refs/heads/release-21.11";
    rev = "cf33704649bb95f44814692048b6a1c74467958b";
  } ) {} }:
pkgs.mkShell {
    buildInputs = [
        	pkgs.ghc
	        pkgs.haskellPackages.haskell-language-server
	        pkgs.stack
    ];
}