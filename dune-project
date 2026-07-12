(lang dune 2.9)

(name cavalry)

(generate_opam_files true)

(source
 (github benmandrew/cavalry))

(authors "Ben Andrew")

(maintainers "Ben Andrew")

(license MIT)

(package
 (name cavalry)
 (synopsis "Toy language for program verification experiments using Hoare logic")
 (description "Toy language for program verification experiments using Hoare logic")
 (depends
  (ocaml (>= "4.14"))
  dune
  core
  astring
  menhir
  why3
  zarith
  cmdliner
  ppx_deriving
  ppx_jane
  (ppx_inline_test :with-test)
  (ppx_assert :with-test)
  (qcheck-core :with-test)
  (odoc :with-doc)
  (ocamlformat
   (and
    (= "0.29.0")
    :with-test))))

;; The browser build (web/) is a separate package so its js_of_ocaml/Z3-wasm
;; toolchain stays out of the core [cavalry] dependencies -- `opam install
;; cavalry` (the CLI/library) does not drag in js_of_ocaml. The dev shell
;; installs both via `opam install --deps-only .`. See web/README.md.
(package
 (name cavalry-web)
 (synopsis "Client-side (browser) build of the Cavalry verifier")
 (description
  "Compiles the Cavalry verification pipeline to JavaScript with js_of_ocaml and pairs it with a WebAssembly build of Z3, so programs can be verified entirely in the browser.")
 (depends
  (ocaml (>= "4.14"))
  dune
  cavalry
  js_of_ocaml
  js_of_ocaml-compiler
  zarith_stubs_js
  yojson
  crunch))

(using menhir 2.1)
