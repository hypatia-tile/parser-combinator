{
  description = "Tiny Haskell dev environment interacting with HLS";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    supportedSystems = [
      "x86_64-linux"
      "aarch64-linux"
      "aarch64-darwin"
    ];

    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

    pkgsFor = system:
      import nixpkgs {
        inherit system;
      };
  in {
    packages = forAllSystems (system: let
      pkgs = pkgsFor system;
      hpkgs = pkgs.haskellPackages;
    in {
      megaparsec-example = hpkgs.callCabal2nix "megaparsec-example" ./megaparsec-example {};
      parser-demo = hpkgs.callCabal2nix "parser-demo" ./parser-demo {};
      megaparsec-tutorial = hpkgs.callCabal2nix "megaparsec-tutorial" ./megaparsec-tutorial {};

      # Chose one default package.
      # This is what `nix build` builds by default.
      default = self.packages.${system}.megaparsec-example;
    });

    devShells = forAllSystems (system: let
      pkgs = pkgsFor system;
      hpkgs = pkgs.haskellPackages;
    in {
      default = hpkgs.shellFor {
        packages = p: [
          self.packages.${system}.megaparsec-example
          self.packages.${system}.parser-demo
          self.packages.${system}.megaparsec-tutorial
        ];

        nativeBuildInputs = [
          hpkgs.ghc
          hpkgs.hoogle
          hpkgs.fast-tags
          hpkgs.cabal-install
          hpkgs.haskell-language-server
          pkgs.fourmolu
        ];
      };
    });
  };
}
