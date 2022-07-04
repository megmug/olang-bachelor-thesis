{ pkgs ? import ( builtins.fetchGit {
    # nixos-21.11 at 2022-05-10
    url = "https://github.com/nixos/nixpkgs/";
    ref = "release-22.05";
    rev = "bd12dea7839162fc6a7a38e6d5c4652dde8ecabd";
  } ) {} }:
pkgs.mkShell {
    buildInputs = [
        	pkgs.ghc
	        pkgs.haskellPackages.haskell-language-server
	        pkgs.stack
    ];
}