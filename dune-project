(lang dune 2.0)
(generate_opam_files true)

(name lwd)
(source (github let-def/lwd))
(license MIT)
(authors "Frédéric Bour")
(maintainers "fred@tarides.com")

(package
 (name lwd)
 (synopsis "Lightweight reactive documents")
 (documentation "https://let-def.github.io/lwd/doc")
 (depends
  (dune (>= 2.0))
  seq
  (ocaml (>= "4.03"))
  (qtest :with-test)
  (qcheck :with-test)))

(package
 (name nottui)
 (synopsis "UI toolkit for the terminal built on top of Notty and Lwd")
 (documentation "https://let-def.github.io/lwd/doc")
 (depends
  (dune (>= 2.0))
  (lwd (= :version))
  notty))

(package
 (name tyxml-lwd)
 (synopsis "Make reactive webpages in Js_of_ocaml using Tyxml and Lwd")
 (documentation "https://let-def.github.io/lwd/doc")
 (depends
  (dune (>= 2.0))
  (lwd (= :version))
  tyxml
  js_of_ocaml
  js_of_ocaml-ppx))

(package
 (name nottui-pretty)
 (synopsis "A pretty-printer based on PPrint rendering UIs")
 (documentation "https://let-def.github.io/lwd/doc")
 (depends
  (dune (>= 2.0))
  notty
  (nottui (= :version))))

(package
 (name nottui-lwt)
 (synopsis "Run Nottui UIs in Lwt")
 (documentation "https://let-def.github.io/lwd/doc")
 (depends
  (dune (>= 2.0))
  notty
  lwt
  (nottui (= :version))))
