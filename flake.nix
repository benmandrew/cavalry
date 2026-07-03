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
            # One-time bootstrap: create the local opam switch if needed, install
            # project deps, and let why3 detect the (opam-built) Alt-Ergo prover.
            # Guarded by a stamp file so it only runs once per clone, not on
            # every shell entry.
            STAMP=.direnv/opam-bootstrap-done
            if [ ! -f "$STAMP" ]; then
              echo "cavalry dev shell: bootstrapping opam switch (one-time)..."
              ( [ -d _opam ] || opam switch create . 5.5.0 -y ) \
                && opam install --deps-only --with-test -y . \
                && why3 config detect \
                && mkdir -p .direnv \
                && touch "$STAMP"
            fi
            eval "$(opam env --switch=. --set-switch 2>/dev/null)"
          '';
        };
      }
    );
}
