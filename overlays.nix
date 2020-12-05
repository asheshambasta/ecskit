# A central overlay file that supplies us with all ca specific packages and global overrides.
# supplies a list of overlays to apply. 
let

  sources = import ./nix/sources.nix;

  # Add overlays here.
  ourOverlays = with sources; [
  ];
  # preludeOverlays = with ca-nixutils.overlays; [
  #   utils
  #   http-client
  #   amazonka
  #   db
  #   wai
  # ];

in # preludeOverlays ++ 
   ourOverlays ++ [ (import ./overlay.nix) ]

