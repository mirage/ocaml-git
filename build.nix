import (builtins.getFlake "nixpkgs") {
    overlays = [
      (self: super: {
        ocamlPackages = super.ocamlPackages.overrideScope' (_: super: {
          carton-git = super.carton-git.overrideAttrs (_: { src = ./.; });
          carton-lwt = super.carton-lwt.overrideAttrs (_: { src = ./.; });
          carton = super.carton.overrideAttrs (_: { src = ./.; });
          git-mirage = super.git-mirage.overrideAttrs (_: { src = ./.; });
          git-paf = super.git-paf.overrideAttrs (_: { src = ./.; });
          git-unix = super.git-unix.overrideAttrs (_: { src = ./.; });
          git = super.git.overrideAttrs (_: { src = ./.; });
        });
      })
    ];
  }
