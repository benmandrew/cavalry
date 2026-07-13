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
        # OCaml and why3 stay opam-managed (see README) so the exact versions
        # pinned in cavalry.opam are what actually get built; this shell provides
        # opam plus the native libs/tools it needs to build those from source.
        # The Z3 prover, by contrast, is a plain binary and not an opam package,
        # so nix supplies it (pinned via flake.lock to 4.16.0, the version
        # [Smt.Prover] checks for); [why3 config detect] then finds it. It is
        # deliberately absent from cavalry.opam's depends -- opam installs no
        # prover.
        #
        # nodejs is for the browser build under web/ (npm: z3-solver, esbuild,
        # puppeteer-core). Its OCaml-side toolchain (js_of_ocaml, crunch, ...) is
        # opam-managed under the separate cavalry-web.opam package, which the
        # `opam install --deps-only .` below installs alongside cavalry's deps.
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            opam
            z3
            nodejs
            m4
            pkg-config
            gmp
            zlib
            perl
          ];

          shellHook = ''
            # opam builds native objects with the system toolchain, which
            # targets the host macOS version, but nix's devShell otherwise pins
            # MACOSX_DEPLOYMENT_TARGET to an older SDK. The mismatch makes the
            # linker warn on every object file; align the target with the host
            # so the two agree.
            if [ "$(uname)" = "Darwin" ]; then
              export MACOSX_DEPLOYMENT_TARGET="$(sw_vers -productVersion | cut -d. -f1).0"
            fi

            # Create the local opam switch on first entry.
            [ -d _opam ] || opam switch create . 5.5.0 -y

            # Activate the switch *before* the steps below, so opam acts on it
            # rather than the global default and so its binaries (why3) are on
            # PATH. This eval used to run last, which meant the
            # bootstrap's `why3 config detect` fired before _opam/bin was on
            # PATH and died with "why3: command not found".
            eval "$(opam env --switch=. --set-switch 2>/dev/null)"

            # Re-sync deps whenever the manifest or this hook changes, not just
            # once per clone. The stamp records a checksum of cavalry.opam and
            # flake.nix, so adding a dependency (say a new with-test library) or
            # changing the install line below re-triggers the install for
            # existing clones instead of leaving them silently drifted.
            STAMP=.direnv/opam-deps.stamp
            WANT=$(cat cavalry.opam cavalry-web.opam flake.nix | cksum | cut -d' ' -f1)
            if [ "$(cat "$STAMP" 2>/dev/null)" != "$WANT" ]; then
              echo "cavalry dev shell: syncing opam deps (manifest changed)..."
              opam install --deps-only --with-test --with-doc -y . \
                && why3 config detect \
                && mkdir -p .direnv \
                && printf '%s' "$WANT" > "$STAMP"
            fi
          '';
        };
      }
    );
}
