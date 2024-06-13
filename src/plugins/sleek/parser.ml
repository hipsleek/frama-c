(* open Cil_types
open Cabs *)

let parse_to_cabs (path : Filepath.Normalized.t) =
  let lexbuf, lexer =
    Lexer.init ~filename:(path:>string) Lexer.initial in
  let cabs = Cparser.file lexer lexbuf in
  (* Cprint.print_defs cabs;*)
  Lexer.finish ();
  (path, cabs)

let custom_parser f =
  let empty_cil : Cil_types.file = {
    fileName = Filepath.Normalized.of_string f;
    globals = [];
    globinit = None;
    globinitcalled = false;
  } in
  (* let filepath : Datatype.Filepath.t = Datatype.Filepath.of_string f in
  let empty_definitions : (bool * definition) list = [] in
  let empty_cabs : Cabs.file = (filepath , empty_definitions) in
  (empty_cil, empty_cabs) *)
  let filepath = Filepath.Normalized.of_string f in
  let empty_cabs = parse_to_cabs filepath in
  (empty_cil, empty_cabs)

(* let custom_parser file =
  let path = Filepath.Normalized.of_string file in
  let cabs = parse_to_cabs path in
  (* Now (return a function that will) convert to CIL *)
  let cil = Cabs2cil.convFile cabs in
  cil,cabs *)
