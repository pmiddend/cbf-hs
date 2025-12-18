{
  description = "cbf";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ ];
          };

          haskellPackages = pkgs.haskellPackages.override
            {
              overrides = self: super: { };
            };

          packageName = "cbf";

        in
        {
          packages.${packageName} =
            haskellPackages.callCabal2nix packageName self { };

          packages.default = self.packages.${system}.${packageName};

          defaultPackage = self.packages.${system}.default;

          devShells.default =
            pkgs.mkShell {
              buildInputs = with pkgs; [
                haskellPackages.haskell-language-server
                cabal-install
                haskellPackages.hlint
              ];
              inputsFrom = [ self.packages.${system}.cbf.env ];
            };
          devShell = self.devShells.${system}.default;
        });
}
