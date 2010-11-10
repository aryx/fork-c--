(defun pad-ocaml-project-c-- ()
  (interactive)

  (setq 
   pad-ocaml-project-path "/home/pad/github/c--"
   pad-ocaml-project-subdirs 
   ; see MAKESUBDIRS in the Makefile
   (split-string 
    "commons commons2 commons3 
     error 
     h_asdl
     parsing

     front_rtl   front_asm  front_fenv
     front_nelab

     front_cfg front_zipcfg
     front_target
     front_ir
     assembler
   ")
   pad-ocaml-project-toplevel "qc.top"
   )

  ; --------------------------------------------------------------------------
  ; qc
  ; --------------------------------------------------------------------------
  (setq
   pad-ocaml-project-prog     "qc"
   pad-ocaml-project-args 
   (join-string 
    (list 
     ;"-debugger"
     (case 3
       (1 "-driver_parse demos/hello.c--")
       (2 "-test_nast demos/hello.c--")
       (3 "-test_nelab demos/hello.c--")
       (4 "-test_nelab demos/forloop_tiger.c--")
       )
     )
    )
   )
  )


