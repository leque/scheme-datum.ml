(lang dune 2.5)
(name scheme-datum)

(generate_opam_files true)

(source (github leque/scheme-datum.ml))
(license MIT)
(authors "OOHASHI Daichi <dico.leque.comicron@gmail.com>")
(maintainers "OOHASHI Daichi <dico.leque.comicron@gmail.com>")

(package
 (name scheme-datum)
 (version "~dev")
 (synopsis "A subset of R7RS Scheme parser")
 (depends
  (ocaml (and (>= 4.07.0) (< 4.13.0)))
  (dune (and :build (>= 2.5) (< 2.9)))
  (base (and (>= v0.14) (< v0.15)))
  (stdio (and :with-test (>= v0.14) (< v0.15)))
  ppx_deriving
  (ppx_let (and :build))
  (uucp (and (>= 13.0) (< 14.0)))
  (sedlex (and (>= 2.3) (< 3.0)))
))
