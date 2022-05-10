{ pkgs ? import ( builtins.fetchGit {
    # nixos-21.11 at 2022-05-10
    url = "https://github.com/nixos/nixpkgs/";
    ref = "release-21.11";
    rev = "aa2f845096f72dde4ad0c168eeec387cbd2eae04";
  } ) {} }:
pkgs.mkShell {
    buildInputs = [
        	pkgs.ghc
	        pkgs.haskellPackages.haskell-language-server
	        pkgs.stack
    ];
}