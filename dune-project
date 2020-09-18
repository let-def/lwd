(lang dune 1.11)
(generate_opam_files true)

(name lwd)
(source (github let-def/lwd))
(license MIT)
(authors "Frédéric Bour")
(maintainers "fred@tarides.com")

(package
 (name lwd)
 (synopsis "Lightweight reactive documents")
 (description "TODO")
 (depends (dune (>= 1.11.0)) seq (ocaml (>= "4.03"))
          (qtest :with-test)
          (qcheck :with-test)))

(package
 (name nottui)
 (synopsis "UI toolkit for the terminal built on top of Notty")
 (description "TODO")
 (depends (dune (>= 1.11.0)) lwd notty))

(package
 (name tyxml-lwd)
 (synopsis "Hello")
 (description "TODO")
 (depends dune lwd tyxml js_of_ocaml))

(package
 (name nottui-pretty)
 (synopsis "A pretty-printer based on PPrint rendering UIs")
 (description "TODO")
 (depends (dune (>= 1.11.0)) notty nottui))

(package
 (name nottui-lwt)
 (synopsis "Run Nottui UIs in Lwt")
 (description "TODO")
 (depends (dune (>= 1.11.0)) notty lwt nottui))
