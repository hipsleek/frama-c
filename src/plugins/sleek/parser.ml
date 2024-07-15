let parse (path : Filepath.Normalized.t) =
  let lexbuf, lexer =
    Lexer.init ~filename:(path:>string) Lexer.initial in
  let cabs = Ccparser.file lexer lexbuf in
  (* Cprint.print_defs cabs;*)
  Lexer.finish ();
  cabs

let custom_parser f =
  let filepath = Filepath.Normalized.of_string f in
  let cil, cabs = Frontc.parse filepath () in
  let cast = parse filepath in
  let () = State.process cast in
  (cil, cabs)

