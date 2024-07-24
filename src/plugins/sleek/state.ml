open Hipsleek_api
open Cabs

exception Troll of string

let cast : (bool * Cabs.definition) list option ref = ref None

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

(***********************
  FORWARD VERIFICATION
************************)
(* update state when encountering a constant expression *)
let update_result c lfe =
  match c with 
  | CONST_INT str -> lfe := Sleekapi.upd_result_with_int !lfe (int_of_string str)
  | _ -> ()

let process_expression (expr : cabsexp) lfe =
  match expr with
  | CONSTANT c -> update_result c lfe
  | _ -> ()

let rec process_statement (stmt : raw_statement) lfe = 
  match stmt with
  | BLOCK (b, _, _) -> process_block b lfe
  | IF (_, stmt1, stmt2, _) -> 
    (* process_expression expr.expr_node lfe *)
    lfe := Sleekapi.upd_result_with_bool !lfe true;
    (* Assignment : update res *)
    lfe := Sleekapi.add_assign_to_ctx !lfe Bool "v_bool";

    (* Cond : then branch *)
    let then_lfe = ref (Sleekapi.add_cond_to_ctx !lfe "v_bool" true) in
    process_statement stmt1.stmt_node then_lfe;

    (* Cond : else branch *)
    let else_lfe = ref (Sleekapi.add_cond_to_ctx !lfe "v_bool" false) in
    process_statement stmt2.stmt_node else_lfe;

    lfe := Sleekapi.disj_of_ctx !then_lfe !else_lfe 

  | RETURN (expr, _) -> 
    process_expression expr.expr_node lfe
  | DEFINITION _ -> ()
  | _ -> ()

(* 
  Walk through function block 
  We don't care about ghost statement yet
  We also don't care about scope in this stage
*)
and process_block (block : block) lfe =
  let stmts = List.filter (fun s -> not s.stmt_ghost) block.bstmts in
  let raw_stmts = List.map (fun stmt -> stmt.stmt_node) stmts in 
  List.iter (fun stmt -> process_statement stmt lfe) raw_stmts

let forwardVerify (cast : (bool * definition) list) =
  let temp_ast = List.filter (fun (b, _) -> not b) cast in (* We don't care about ghost code *)
  let ast = List.map (fun (_, d) -> d) temp_ast in         (* We reduce the AST to a list of definition *)
  List.iter (Cabs_debug.pp_def Format.std_formatter) ast;
  let rec verify (spec : string) (d : definition) =
    match d with 
    | SLEEK_FUNDEF (s, fundef) -> verify s fundef
    | FUNDEF (_, name, function_block, _, _) -> 
      let function_name, _, _, _ = snd name in 
      let cstruc_form = Sleekapi.spec_decl function_name spec [] in
      let lfe = ref (Sleekapi.init_ctx cstruc_form []) in
      let () = process_block function_block lfe in
      Format.printf "CHECK ENTAIL RESULT: %b\n" (Sleekapi.check_entail_post !lfe cstruc_form [])

      (* 
        Struct declaration has been translated to Data declaration in the parsing step.
        However, I will move that piece code here in the future for consistency!
      *)
    | DECDEF (_, _, _) | TYPEDEF (_, _) | ONLYTYPEDEF (_, _) 
    | GLOBASM (_, _)
    | PRAGMA (_, _)
    | STATIC_ASSERT (_, _, _) 
    | LINKAGE (_, _, _)
    | GLOBANNOT (_) -> ()
  in
  List.iter (verify "") ast
  