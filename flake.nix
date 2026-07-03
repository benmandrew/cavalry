{
  description = "Cavalry: a toy imperative language with Hoare-logic verification";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        # OCaml/why3/alt-ergo themselves stay opam-managed (see README) so the
        # exact versions pinned in cavalry.opam are what actually get built;
        # this shell only provides opam plus the native libs/tools it needs
        # to build those packages from source.
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            opam
            m4
            pkg-config
            gmp
            perl
          ];

          shellHook = ''
            echo "cavalry dev shell: run 'opam install --deps-only --with-test .' then 'why3 config detect' if you haven't yet."
          '';
        };
      }
    );
}
