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
      url = "github:jeffutter/apollo-router-flake/v1.61.4";
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
        craneLib = crane.mkLib pkgs;

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
                clang
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
              # rover
              (rover.overrideAttrs (
                finalAttrs: prevAttrs: rec {
                  version = "0.28.1";
                  src = fetchFromGitHub {
                    owner = prevAttrs.src.owner;
                    repo = prevAttrs.src.repo;
                    rev = "v${version}";
                    sha256 = "sha256-y+YtNV+Pzb9TxvZ9QpWNk0+qfjEy5TTMWUJMaACe3kI=";
                  };
                  # cargoDeps = prevAttrs.cargoDeps.overrideAttrs (_: {
                  #   inherit src;
                  #   outputHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
                  # });
                  cargoDeps = pkgs.rustPlatform.fetchCargoVendor {
                    inherit (finalAttrs) pname src version;
                    hash = "sha256-8KQ8ahKgg/F75qYY4Kf2J8zywUoSLi3DtGHaJCD/Zpc=";
                  };
                  cargoTestFlags = [
                    "--"
                    "--skip=latest_plugins_are_valid_versions"
                    "--skip=command::docs::shortlinks::tests::each_url_is_valid"
                  ];
                }
              ))
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
