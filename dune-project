(lang dune 3.4)
(using mdx 0.3)
(generate_opam_files true)

(license ISC)
(authors "Andy Ray")

(package
  (name reedsolomon)
  (synopsis "Reed-Solomon error correction CODEC")
  (description "Reed-Solomon based encoder and decoder with model and hardware design.")
  (depends
    hardcaml
    (hardcaml_waveterm :with-test)
    core
    ppx_deriving_hardcaml
    ))

