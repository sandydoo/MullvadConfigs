{
  description = "Generate Wireguard configurations for Mullvad";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
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

        project = { returnShellEnv }: hpkgs.developPackage {
          name = "MullvadConfigs";
          root = ./.;
          withHoogle = false;

          # Allow a shell environment to be requested
          inherit returnShellEnv;

          modifier = drv: hlib.addBuildTools drv (with hpkgs;
            [
              cabal-fmt
              cabal-install
              ghcid
              haskell-language-server
              ormolu
              pkgs.nixpkgs-fmt
            ]);
        };
      in
      {
        defaultPackage = project { returnShellEnv = false; };
        devShell = project { returnShellEnv = true; };
      }
    );
}
