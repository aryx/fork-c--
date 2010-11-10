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
     (case 40
       (1 "-driver_parse demos/hello.c--")
       (2 "-test_nast demos/hello.c--")
       (3 "-test_nelab demos/hello.c--")
       (4 "-test_nelab demos/forloop_tiger.c--")

       (10 "-test_nast demos/bool.c--")
       (11 "-test_nelab demos/bool.c--")

       (20 "-driver_compile demos/hello.c--")
       
       (40 "-test_x86 demos/bool.c--")
       )
     )
    )
   )


  )


