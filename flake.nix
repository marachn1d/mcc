{
  description = "Development environment for mcc";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url  = "github:numtide/flake-utils";
  };

  outputs = { self , nixpkgs , rust-overlay, flake-utils, ... }:
  flake-utils.lib.eachDefaultSystem (system:
    let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in
      {
        devShells.default = with pkgs; mkShell {
          buildInputs = [
	    python3
	    binutils_nogold
            rust-bin.stable.latest.complete
          ];

          shellHook = ''
            zsh
          '';
        };
      }
    );

}
