(lang dune 3.16)

(name cron)

(generate_opam_files true)

(source
 (github syaiful6/cron.ml))

(authors "Syaiful Bahri")

(maintainers "syai.bahri6@gmail.com")

(license "MIT")

(documentation https://url/to/documentation)

(package
 (name cron)
 (synopsis "Cron data structure and parser")
 (description "Cron data structure and parser.")
 (depends
  ocaml
  dune
  (angstrom
   (>= 0.16.0))
  ptime
  (alcotest :with-test)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
