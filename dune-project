(lang dune 3.7)
(name cmm)

; Declaring a package is what lets the "main" executable below carry a
; (public_name qc) and be picked up by `dune install`/`dune build @install`.
; Only bin/qc is installed (no findlib libraries are made public: all the
; cmm_* libraries stay internal build-time pieces, not an installable API).
(package
 (name cmm)
 (synopsis "Quick C--, a retargetable C-- compiler")
 (description "OCaml port/fork of Quick C--, see http://www.cminusminus.org")
 (authors "Yoann Padioleau <yoann.padioleau@gmail.com>")
 (maintainers "Yoann Padioleau <yoann.padioleau@gmail.com>")
 (depends
  (ocaml (>= 4.14.0))
  ; used directly by our own code: -dump_ast and friends in driver/main.ml,
  ; and the [@@deriving show] annotations in ast.ml / nast.ml.
  ppx_deriving
  ; the commons/profiling libraries come from the semgrep-pfff-libs
  ; submodule rather than opam, so their own dependencies are installed
  ; separately in the Dockerfile from the .opam files they ship.
  num))

; Regenerate cmm.opam from the metadata above whenever it's out of date
; (e.g. after `dune build`), instead of hand-maintaining an .opam file.
(generate_opam_files)
