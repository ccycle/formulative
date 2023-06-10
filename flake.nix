{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    stack2cabal.url = "github:hasufell/stack2cabal";
  };

  outputs = { self, nixpkgs, flake-utils, stack2cabal }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ghc-version = "ghc927";
        pkgs = import nixpkgs {
          inherit system;
          config = { allowUnfree = true; };
        };
        stack2cabalOverrided = removeAttrs stack2cabal [ "nixConfig" ];

        buildInputs = [
          pkgs.cabal-install
          pkgs.go-task
          pkgs.haskell-language-server
          pkgs.haskell.compiler.${ghc-version}
          pkgs.haskell.packages.${ghc-version}.fourmolu
          pkgs.hpack
          pkgs.nil
          pkgs.nixpkgs-fmt
          pkgs.stack
          pkgs.zlib
          stack2cabalOverrided.defaultPackage.${system}
        ];
      in
      {
        devShell =
          pkgs.mkShell {
            inherit buildInputs;
          };
      }
    );
}
