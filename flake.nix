{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        lib = nixpkgs.lib;
      in
      lib.foldl' lib.recursiveUpdate { }
        ([
          {
            packages.default = self.packages.${system}."generics-eot_ghc-9-12-2";
            devShells.default = self.devShells.${system}."generics-eot_ghc-9-12-2";
          }
        ] ++
        (lib.map
          (haskellPackages:
            let
              haskellPackage = haskellPackages.developPackage {
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
                modifier = drv:
                  pkgs.haskell.lib.appendConfigureFlags drv [
                    "--ghc-option=-Werror"
                  ];
              };
              ghcWithDeps = haskellPackages.ghc.withPackages (p: haskellPackage.buildInputs);
              ghcName = lib.replaceStrings [ "." ] [ "-" ] haskellPackages.ghc.version;
            in
            {
              packages."generics-eot_ghc-${ghcName}" = haskellPackage;
              devShells."generics-eot_ghc-${ghcName}" = pkgs.mkShell {
                buildInputs = [
                  ghcWithDeps
                  haskellPackages.hpack
                  haskellPackages.cabal-install
                  haskellPackages.haskell-language-server
                ];
              };
            })
          [
            pkgs.haskell.packages.ghc96
            pkgs.haskell.packages.ghc98
            pkgs.haskell.packages.ghc910
            pkgs.haskell.packages.ghc912
          ]
        )));
}
