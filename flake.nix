{
  description = "cl-coverage-tools flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-25.05";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in {

        packages.${system} = {
          default = pkgs.mkShell {
            name = "cl-coverage-tools";
            buildInputs = [pkgs.sbcl];
          };
        };

        devShell = pkgs.mkShell {
            name = "cl-coverage-tools";
            buildInputs = [pkgs.sbcl];
        };
      });
}
