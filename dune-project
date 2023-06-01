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
  (ocaml (>= :"4.08"))
  dune
  core
  astring
  menhir
  why3
  (alt-ergo (= :"2.4.3"))
  ppx_deriving))

(using menhir 2.1)
