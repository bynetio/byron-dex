let
  packages = import ./.;

  pkgs = packages.pkgs;
  haskellNix = pkgs.haskell-nix;

  projectPackages =
    haskellNix.haskellLib.selectProjectPackages packages.project.hsPkgs;

  inherit (import ./nix/lib/build.nix { inherit pkgs; }) dimension;

  # Collects haskell derivations and builds an attrset:
  #
  # { library = { ... }
  # , tests = { ... }
  # , benchmarks = { ... }
  # , exes = { ... }
  # , checks = { ... }
  # }
  #  Where each attribute contains an attribute set
  #  with all haskell components of that type
  mkHaskellDimension = pkgs: haskellProjects:
    let
      # retrieve all checks from a Haskell package
      collectChecks = _: ps: pkgs.haskell-nix.haskellLib.collectChecks' ps;
      # retrieve all components of a Haskell package
      collectComponents = type: ps:
        pkgs.haskell-nix.haskellLib.collectComponents' type ps;
      # Given a component type and the retrieve function, retrieve components from haskell packages
      select = type: selector: (selector type) haskellProjects;
      # { component-type : retriever-fn }
      attrs = {
        "library" = collectComponents;
        "tests" = collectComponents;
        "benchmarks" = collectComponents;
        "exes" = collectComponents;
        "checks" = collectChecks;
      };
    in
    dimension "Haskell component" attrs select;

  mkDockerImage = pkgs: package: imageName: execName:
    pkgs.dockerTools.buildImage {
      name = imageName;
      tag = "latest";
      contents = [ package pkgs.bash ];
      config = { Cmd = [ "/bin/${execName}" ]; };
    };

  build = pkgs.recurseIntoAttrs (mkHaskellDimension pkgs projectPackages);
  exes = build.exes;

  pab-exec = exes.dex."dex-pab";
  middleware-exec = exes.middleware."middleware-manual";

in
{
  backend = mkDockerImage pkgs pab-exec "dex-backend" "dex-pab";
  middleware =
    mkDockerImage pkgs middleware-exec "dex-middleware" "midleware-manual";
}
