  $ cat clipper.caramel
  @derive(Debug, Default)
  @clipper(
    name = "Test CLI",
    desc = "A sample test cli"
  )
  type t =
    | Print {
        @clipper(short="m", long="message", env= "MESSAGE")
        message: string,
      }
  
    | @clipper(name = "file")
      Print_file {
        @clipper(name = "PRINT_FILE")
        file: OS.Path.t
      }
  
  pub fn hello(name) {
    :ok
  }

  $ caramel parse --file clipper.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_type
                      ((typ_name (Id (t))) (typ_args ())
                        (typ_desc
                          (Type_variant
                            (tyk_constructors
                              (((ctr_name (Id (Print)))
                                 (ctr_args
                                   (Record
                                     (((lbl_name (Id (message)))
                                        (lbl_type (Type_name (Id (string))))
                                        (lbl_annot
                                          (((ann_name (Id (clipper)))
                                             (ann_desc
                                               ((Map
                                                  (((Id (short)) (m))
                                                    ((Id (long)) (message))
                                                    ((Id (env)) (MESSAGE)))))))))))))
                                 (ctr_annot ()))
                                ((ctr_name (Id (Print_file)))
                                  (ctr_args
                                    (Record
                                      (((lbl_name (Id (file)))
                                         (lbl_type
                                           (Type_name (Id (OS Path t))))
                                         (lbl_annot
                                           (((ann_name (Id (clipper)))
                                              (ann_desc
                                                ((Map
                                                   (((Id (name)) (PRINT_FILE)))))))))))))
                                  (ctr_annot
                                    (((ann_name (Id (clipper)))
                                       (ann_desc
                                         ((Map (((Id (name)) (file))))))))))))))
                        (typ_annot
                          (((ann_name (Id (derive)))
                             (ann_desc
                               ((Map (((Id (Debug)) ()) ((Id (Default)) ()))))))
                            ((ann_name (Id (clipper)))
                              (ann_desc
                                ((Map
                                   (((Id (name)) ("Test CLI"))
                                     ((Id (desc)) ("A sample test cli")))))))))))
                     (Str_fun
                       ((fn_visibility Public) (fn_name (Id (hello)))
                         (fn_args ((No_label (Pat_bind (Id (name))))))
                         (fn_arity 1) (fn_body (Expr_literal (Lit_atom ok)))
                         (fn_annot ()))))
