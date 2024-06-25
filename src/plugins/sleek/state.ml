type custom_ast = {
  original_ast : Cil_types.file option ref;
  sleek_specs : string ref;
}

let state : custom_ast = {
  original_ast = ref None;
  sleek_specs = ref "";
}

let get_sleek_specs () = !(state.sleek_specs)

let update_sleek_specs (state : custom_ast) (specs : string) =
  let current_specs = get_sleek_specs () in 
  state.sleek_specs := current_specs ^ specs

let update_ast (ast : Cil_types.file) =
  state.original_ast := Some ast

let print_specs () = print_string (get_sleek_specs ())