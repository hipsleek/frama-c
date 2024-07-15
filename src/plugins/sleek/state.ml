(*
  THIS IS WHERE THE CUSTOM AST WILL BE THROUGH A FORWARD VERIFICATION
*)

open Hipsleek_api
open Cabs

exception Troll of string

let cast : (bool * Cabs.definition) list option ref = ref None

let process ast =
  cast := Some ast

(* 
  CONVERTING STRUCT TO DATA 
*)
let add lst type_spec str = 
  match type_spec with
    | Tvoid -> lst := !lst @ [(Sleekapi.Void, str)]
    | Tbool -> lst := !lst @ [(Sleekapi.Bool, str)]
    | Tint -> lst := !lst @ [(Sleekapi.Int, str)]
    | Tfloat -> lst := !lst @ [(Sleekapi.Float, str)]
    | Tstruct (name, _, _) -> lst := !lst @ [(Sleekapi.Named(name), str)]
    | _ -> ()

let add_list lst spec str =
  match spec with 
  | SpecType type_spec -> ignore(add lst type_spec str)
  | _ -> ()

let rec fill_list lst spec l = 
  match spec with
  | [] -> ()
  | spec_h :: spec_t -> 
    match l with
    | [] -> raise (Troll "not the same length")
    | ((str, _, _, _), _) :: l_t ->
      add_list lst spec_h str;
      fill_list lst spec_t l_t

let struct_decl (data_name : string) (field_list : field_group list) =
  let data_fields = ref [] in
  let () = List.iter (function
    | FIELD (spec, l) -> fill_list data_fields spec l
    | STATIC_ASSERT_FG (_, _, _) -> ()
  ) field_list in
  Sleekapi.data_decl data_name !data_fields