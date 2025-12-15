{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        lib = nixpkgs.lib;
        haskellPackage = pkgs.haskellPackages.developPackage {
          root = lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              ./generics-eot.cabal
              ./src
              ./test
              ./examples
              ./docs/tutorial.md
              ./LICENSE
            ];
          };
        };
        ghcWithDeps = pkgs.haskellPackages.ghc.withPackages (p: haskellPackage.buildInputs);
      in
      {
        packages = {
          default = haskellPackage;
        };
        devShells = {
          default = pkgs.mkShell {
            buildInputs = [
              ghcWithDeps
              pkgs.haskellPackages.hpack
              pkgs.cabal-install
              pkgs.haskell-language-server
            ];
          };
        };
      });
}
