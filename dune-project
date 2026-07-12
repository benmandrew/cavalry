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

(using menhir 2.1)
