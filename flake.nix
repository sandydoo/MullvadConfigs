{
  description = "Generate Wireguard configurations for Mullvad";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";

   outputs = { self, nixpkgs, flake-utils }:
		flake-utils.lib.eachSystem ["x86_64-darwin" "x86_64-linux"]
		  (system:
        let pkgs = import nixpkgs { inherit system; }; in
        {
          defaultPackage = pkgs.haskellPackages.callPackage ./default.nix {};
        }
      );
}
