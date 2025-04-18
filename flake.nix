{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    crane = {
      url = "github:ipetkov/crane";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
    apollo-router = {
      url = "github:jeffutter/apollo-router-flake/v1.61.1";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      rust-overlay,
      crane,
      apollo-router,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs { inherit system overlays; };

        lib = nixpkgs.lib;
        craneLib = crane.lib.${system};

        src = lib.cleanSourceWith { src = craneLib.path ./.; };

        envVars =
          { }
          // (lib.attrsets.optionalAttrs pkgs.stdenv.isLinux {
            RUSTFLAGS = "-Clinker=clang -Clink-arg=--ld-path=${pkgs.mold}/bin/mold";
          });

        commonArgs = (
          {
            inherit src;
            buildInputs =
              with pkgs;
              [
                rust-bin.stable.latest.default
                cargo
                rust-analyzer
                rustc
              ]
              ++ lib.optionals stdenv.isDarwin [ libiconv ];
          }
          // envVars
        );
        cargoArtifacts = craneLib.buildDepsOnly commonArgs;

        bin = craneLib.buildPackage (commonArgs // { inherit cargoArtifacts; });
      in
      with pkgs;
      {
        packages = {
          default = bin;
        };

        devShells.default = mkShell (
          {
            packages = [
              rust-bin.stable.latest.default
              cargo
              cargo-watch
              rust-analyzer
              rustc
              rustfmt
              apollo-router.packages.${system}.default
              rover
              mprocs
              entr
            ];
          }
          // envVars
        );

        formatter = nixpkgs-fmt;
      }
    );
}
