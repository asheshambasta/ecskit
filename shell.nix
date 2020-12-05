let

  sources = import ./nix/sources.nix;
  overlays = import ./overlays.nix;

  nixpkgs-overlayed = (import sources.nixpkgs) { inherit overlays; };

in nixpkgs-overlayed.haskellPackages.shellFor {
  packages = p: [
    p.ecs-kit
  ];
  buildInputs =
  (with nixpkgs-overlayed.haskellPackages; [ cabal-install ghcid ])
  ++ (with nixpkgs-overlayed; [ inotify-tools ]);
}
