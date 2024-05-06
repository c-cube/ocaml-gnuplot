(lang dune 2.0)
(name gnuplot)
(generate_opam_files true)
(license "LGPL-2.1+ with OCaml linking exception")

(maintainers "simon cruanes")
(authors "Oliver Gu <gu.oliver@yahoo.com>")
(source (github c-cube/ocaml-gnuplot))

(version 0.7)
(package
  (name gnuplot)
  (synopsis
"Simple interface to Gnuplot

Gnuplot-OCaml provides a simple interface to Gnuplot from OCaml.
The API supports only 2D graphs and was inspired by FnuPlot")
  (depends
    base-unix
    ISO8601
    conf-gnuplot
    (ocaml (>= 4.08))
    (odoc :with-doc)))
