(lang dune 2.9)

(name wacc)

(generate_opam_files true)

(source
 (github confucianzuoyuan/wacc))

(authors "Yuan Zuo <zuoyuantc@126.com>")

(maintainers "Yuan Zuo <zuoyuantc@126.com>")

(license MIT)

(package
 (name wacc)
 (synopsis "Write A C Compiler")
 (description
  "implementing a compiler for a subset of c. targeting to x86-64.")
 (depends
  ocaml
  dune
  batteries
  bisect_ppx
  ppx_deriving
  ppx_inline_test
  cmdliner
  camlp-streams))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
