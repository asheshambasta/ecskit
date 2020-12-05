let

  sources = import ./nix/sources.nix;
  overlays = import ./overlays.nix;
  nixpkgs-overlayed = (import sources.nixpkgs) { inherit overlays; };

in {
  inherit (nixpkgs-overlayed.haskellPackages) ecs-kit;
}
