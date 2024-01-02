{
  description = "Generate Wireguard configurations for Mullvad";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowBroken = true;
        };

        hlib = pkgs.haskell.lib;
        hpkgs = pkgs.haskellPackages;

        project = { devMode, returnShellEnv }: hpkgs.developPackage {
          name = "MullvadConfigs";
          root = ./.;
          withHoogle = false;

          # Allow a shell environment to be requested
          inherit returnShellEnv;

          cabal2nixOptions = if devMode then "--flag=dev" else "";

          modifier = drv: hlib.addBuildTools drv (with hpkgs;
            [
              cabal-fmt
              cabal-install
              ghcid
              haskell-language-server
              fourmolu
              pkgs.nixpkgs-fmt
            ]);
        };
      in
      {
        packages.default = project { devMode = false; returnShellEnv = false; };
        devShells.default = project { devMode = true; returnShellEnv = true; };
      }
    );
}
