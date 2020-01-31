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
 (depends dune seq (ocaml (>= "4.03"))))

(package
 (name nottui)
 (synopsis "UI toolkit for the terminal built on top of Notty")
 (description "TODO")
 (depends dune lwd notty fmt))

(package
 (name nottui-lwt)
 (synopsis "Run Nottui UIs in Lwt")
 (description "TODO")
 (depends dune notty lwt nottui))

(package
 (name nottui-widgets)
 (synopsis "A set of widget-like abstractions for Nottui")
 (description "TODO")
 (depends dune nottui))
