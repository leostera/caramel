  $ cat clipper.caramel
  @derive(Debug, Default)
  @clipper(
    name = "Test CLI",
    desc = "A sample test cli"
  )
  type t =
    | Print {
        @clipper(name = "MESSAGE")
        message: string,
      }
  
    | @clipper(name = "file")
      Print_file {
        @clipper(name = "PRINT_FILE", parser = Clipper.Parser.file)
        file: OS.Path.t
      }

  $ caramel parse --file clipper.caramel --dump-tokens --debug
  caramel: [DEBUG] (At (Id derive) Parens_left (Id Debug) Comma (Id Default)
                     Parens_right At (Id clipper) Parens_left (Id name) Equal
                     (String "Test CLI") Comma (Id desc) Equal
                     (String "A sample test cli") Parens_right Type (Id t)
                     Equal Pipe (Id Print) Brace_left At (Id clipper)
                     Parens_left (Id name) Equal (String MESSAGE) Parens_right
                     (Id message) Colon (Id string) Comma Brace_right Pipe At
                     (Id clipper) Parens_left (Id name) Equal (String file)
                     Parens_right (Id Print_file) Brace_left At (Id clipper)
                     Parens_left (Id name) Equal (String PRINT_FILE) Comma
                     (Id parser) Equal (Id Clipper.Parser.file) Parens_right
                     (Id file) Colon (Id OS.Path.t) Brace_right)
