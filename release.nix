{
  system ? builtins.currentSystem,
  doCheck ? true,
}:
let
  flakeLock = builtins.fromJSON (builtins.readFile ./flake.lock);
  fetchGithub =
    flakeInput:
    builtins.fetchTarball {
      url = "https://github.com/${flakeLock.nodes.${flakeInput}.locked.owner}/${
        flakeLock.nodes.${flakeInput}.locked.repo
      }/archive/${flakeLock.nodes.${flakeInput}.locked.rev}.tar.gz";
      sha256 = flakeLock.nodes.${flakeInput}.locked.narHash;
    };
  nixpkgsSrc = fetchGithub "nixpkgs";
  treefmtSrc = fetchGithub "treefmt-nix";

  pkgs = import nixpkgsSrc {
    inherit system;
    overlays = [
      (import ./nix/overlays)
      (_final: prev: {
        treefmt-nix = import treefmtSrc;
        cron = prev.cron.overrideScope (
          _final': _prev': {
            inherit doCheck;
          }
        );
      })
      (import ./nix/overlays/development.nix)
    ];
  };

  ocamlPackageSets = [
    "ocamlPackages"
    "ocamlPackages_4_14"
    "ocamlPackages_5_4"
    "ocamlPackages_5_3"
    "ocamlPackages_5_2"
  ];
  packageNames = [
    "cron"
  ];
  outputs = pkgs.lib.genAttrs ocamlPackageSets (
    ocamlPackages: pkgs.lib.genAttrs packageNames (package: pkgs.ocaml-ng.${ocamlPackages}.${package})
  );
in
outputs
// {
  inherit (pkgs.ocamlPackages)
    cron
    ;
  inherit (pkgs.cron)
    dev-shell
    ;
  checks.formatting = pkgs.cron.checks.formatting;
}
