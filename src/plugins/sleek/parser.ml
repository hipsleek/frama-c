open State

let parse_to_cabs (path : Filepath.Normalized.t) =
  let lexbuf, lexer =
    Lexer.init ~filename:(path:>string) Lexer.initial in
  let cabs = Cparser.file lexer lexbuf in
  (* Cprint.print_defs cabs;*)
  Lexer.finish ();
  (path, cabs)

let custom_parser f =
  let filepath = Filepath.Normalized.of_string f in
  let cabs = parse_to_cabs filepath in
  let cil = Cabs2cil.convFile cabs in
  update_ast cil;
  (cil, cabs)
