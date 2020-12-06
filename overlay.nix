self: super:
let

  hlib = super.haskell.lib;
  lib = super.lib;
  sources = import ./nix/sources.nix; 
  gitignore = (import sources.gitignore {}).gitignoreSource;

  kitOverrides = selfh: superh: {
    prelude-polysemy =  selfh.callCabal2nix "prelude-polysemy" "${sources.prelude-polysemy}/" {};
    ecs-kit = 
      selfh.callCabal2nix "ecskit" (gitignore ./.) { };
  };
in {
  haskellPackages = super.haskellPackages.override (old: {
    overrides =
      lib.composeExtensions (old.overrides or (_: _: { })) kitOverrides;
  });
}
