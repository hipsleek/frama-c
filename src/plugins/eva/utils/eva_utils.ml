(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2024                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(* Callstacks related functions *)

let current_callstack : Callstack.t option ref = ref None

let clear_call_stack () =
  match !current_callstack with
  | None -> ()
  | Some cs ->
    Eva_perf.stop_doing cs;
    current_callstack := None

let init_call_stack kf =
  assert (!current_callstack = None);
  let cs = Callstack.init kf in
  current_callstack := Some cs;
  Eva_perf.start_doing cs;
  cs

let current_call_stack_opt () = !current_callstack

let current_call_stack () =
  match !current_callstack with
  | None -> Self.fatal "Callstack not initialized"
  | Some cs -> cs

let current_kf () =
  let cs = current_call_stack () in
  Callstack.top_kf cs

let push_call_stack kf stmt =
  let cs = current_call_stack () in
  let new_cs = Callstack.push kf stmt cs in
  current_callstack := Some new_cs;
  Eva_perf.start_doing new_cs

let pop_call_stack () =
  let cs = current_call_stack () in
  Eva_perf.stop_doing cs;
  current_callstack := Callstack.pop cs

let pp_callstack fmt =
  if Parameters.PrintCallstacks.get () then
    match !current_callstack with
    | None -> () (* Stack not initialized; happens when handling global initializations *)
    | Some cs ->
      Format.fprintf fmt "@ stack: %a" Callstack.pretty cs


(* Assertions emitted during the analysis *)

let emitter =
  Emitter.create
    "Eva"
    [ Emitter.Property_status; Emitter.Alarm ]
    ~correctness:Parameters.parameters_correctness
    ~tuning:Parameters.parameters_tuning

let get_slevel kf =
  try Parameters.SlevelFunction.find kf
  with Not_found -> Parameters.SemanticUnrollingLevel.get ()

let get_subdivision_option stmt =
  try
    let kf = Kernel_function.find_englobing_kf stmt in
    Parameters.LinearLevelFunction.find kf
  with Not_found -> Parameters.LinearLevel.get ()

let get_subdivision stmt =
  match Eva_annotations.get_subdivision_annot stmt with
  | [] -> get_subdivision_option stmt
  | [x] -> x
  | x :: _ ->
    Self.warning ~current:true ~once:true
      "Several subdivision annotations at the same statement; selecting %i\
       and ignoring the others." x;
    x

let pretty_actuals fmt actuals =
  let pp fmt (e,x) = Cvalue.V.pretty_typ (Some (Cil.typeOf e)) fmt x in
  Pretty_utils.pp_flowlist pp fmt actuals

let pretty_current_cfunction_name fmt =
  Kernel_function.pretty fmt (current_kf())

let warning_once_current fmt =
  Self.warning ~current:true ~once:true fmt

(* Emit alarms in "non-warning" mode *)
let alarm_report ?current ?source ?emitwith ?echo ?once ?append =
  Self.warning ~wkey:Self.wkey_alarm
    ?current ?source ?emitwith ?echo ?once ?append

module DegenerationPoints =
  Cil_state_builder.Stmt_hashtbl
    (Datatype.Bool)
    (struct
      let name = "Eva_utils.Degeneration"
      let size = 17
      let dependencies = [ Self.state ]
    end)

let protect_only_once = ref true

let protect f ~cleanup =
  let catch () = !protect_only_once && not (Kernel.SaveState.is_empty ()) in
  let cleanup () =
    Self.feedback ~once:true "Clean up and save partial results.";
    try cleanup ()
    with e ->
      protect_only_once := false;
      raise e
  in
  try f ();
  with
  | Log.AbortError _ | Log.AbortFatal _ | Log.FeatureRequest _
  | Sys.Break as e when catch () ->
    cleanup ();
    raise e

let register_new_var v typ =
  if Cil.isFunctionType typ then
    Globals.Functions.replace_by_declaration (Cil.empty_funspec()) v v.vdecl
  else
    Globals.Vars.add_decl v

let create_new_var name typ =
  let vi = Cil.makeGlobalVar ~source:false ~temp:false name typ in
  register_new_var vi typ;
  vi

let is_const_write_invalid typ = Cil.typeHasQualifier "const" typ

let find_return_var kf =
  match (Kernel_function.find_return kf).skind with
  | Return (Some ({enode = Lval ((Var vi, NoOffset))}), _) -> Some vi
  | _ | exception Kernel_function.No_Statement -> None

(* Find if a postcondition contains [\result] *)
class postconditions_mention_result = object
  inherit Visitor.frama_c_inplace

  method! vterm_lhost = function
    | TResult _ -> raise Exit
    | _ -> Cil.DoChildren
end
let postconditions_mention_result spec =
  let vis = new postconditions_mention_result in
  let aux_bhv bhv =
    let aux (_, post) = ignore (Visitor.visitFramacIdPredicate vis post) in
    List.iter aux bhv.b_post_cond
  in
  try
    List.iter aux_bhv spec.spec_behavior;
    false
  with Exit -> true

let conv_comp op =
  let module C = Abstract_interp.Comp in
  match op with
  | Eq -> C.Eq
  | Ne -> C.Ne
  | Le -> C.Le
  | Lt -> C.Lt
  | Ge -> C.Ge
  | Gt -> C.Gt
  | _ -> assert false

let conv_relation rel =
  let module C = Abstract_interp.Comp in
  match rel with
  | Req -> C.Eq
  | Rneq -> C.Ne
  | Rle -> C.Le
  | Rlt -> C.Lt
  | Rge -> C.Ge
  | Rgt -> C.Gt

let loc_dummy_value =
  let l = { Cil_datatype.Position.unknown with
            Filepath.pos_path = Datatype.Filepath.of_string "_value_" }
  in
  l, l

let zero e =
  let loc = loc_dummy_value in
  let typ = Cil.unrollType (Cil.typeOf e) in
  match typ with
  | TFloat (fk, _) -> Cil.new_exp ~loc (Const (CReal (0., fk, None)))
  | TEnum ({ekind = ik },_)
  | TInt (ik, _) -> Cil.new_exp ~loc (Const (CInt64 (Integer.zero, ik, None)))
  | TPtr _ ->
    let ik = Cil.(theMachine.upointKind) in
    let zero = Cil.new_exp ~loc (Const (CInt64 (Integer.zero, ik, None))) in
    Cil.mkCast ~force:true ~newt:typ zero
  | typ -> Self.fatal ~current:true "non-scalar type %a"
             Printer.pp_typ typ

let eq_with_zero positive e =
  let op = if positive then Eq else Ne in
  let loc = Cil_datatype.Location.unknown in
  Cil.new_exp ~loc (BinOp (op, zero e, e, Cil.intType))

let is_value_zero e =
  e.eloc == loc_dummy_value

let inv_rel = function
  | Gt -> Le
  | Lt -> Ge
  | Le -> Gt
  | Ge -> Lt
  | Eq -> Ne
  | Ne -> Eq
  | _ -> assert false

(* Transform an expression supposed to be [positive] into an equivalent
   one in which the root expression is a comparison operator. *)
let rec normalize_as_cond expr positive =
  match expr.enode with
  | UnOp (LNot, e, _) -> normalize_as_cond e (not positive)
  | BinOp ((Le|Ne|Eq|Gt|Lt|Ge as binop), e1, e2, typ) ->
    if positive then
      expr
    else
      let binop = inv_rel binop in
      let enode = BinOp (binop, e1, e2, typ) in
      Cil.new_exp ~loc:expr.eloc enode
  | _ ->
    eq_with_zero (not positive) expr

module PairExpBool =
  Datatype.Pair_with_collections(Cil_datatype.Exp)(Datatype.Bool)
    (struct let module_name = "Value.Eva_utils.PairExpBool" end)
module MemoNormalizeAsCond =
  State_builder.Hashtbl
    (PairExpBool.Hashtbl)
    (Cil_datatype.Exp)
    (struct
      let name = "Eva_utils.MemoNormalizeAsCond"
      let size = 64
      let dependencies = [ Ast.self ]
    end)
let normalize_as_cond e pos =
  MemoNormalizeAsCond.memo (fun (e, pos) -> normalize_as_cond e pos) (e, pos)

module MemoLvalToExp =
  Cil_state_builder.Lval_hashtbl
    (Cil_datatype.Exp)
    (struct
      let name = "Eva_utils.MemoLvalToExp"
      let size = 64
      let dependencies = [ Ast.self ]
    end)

let lval_to_exp =
  MemoLvalToExp.memo
    (fun lv -> Cil.new_exp ~loc:Cil_datatype.Location.unknown (Lval lv))

(* Computation of the inputs of an expression. *)
let rec deps_of_expr find_loc expr =
  let rec process expr = match expr.enode with
    | Lval lval ->
      (* Dereference of an lvalue. *)
      deps_of_lval find_loc lval
    | UnOp (_, e, _) | CastE (_, e) ->
      (* Unary operators. *)
      process e
    | BinOp (_, e1, e2, _) ->
      (* Binary operators. *)
      Deps.join (process e1) (process e2)
    | StartOf lv | AddrOf lv ->
      (* computation of an address: the inputs of the lvalue whose address
         is computed are read to compute said address. *)
      { data = indirect_zone_of_lval find_loc lv;
        indirect = Locations.Zone.bottom; }
    | Const _ | SizeOf _ | AlignOf _ | SizeOfStr _ | SizeOfE _ | AlignOfE _ ->
      (* static constructs, nothing is read to evaluate them. *)
      Deps.bottom
  in
  process expr

and zone_of_expr find_loc expr = Deps.to_zone (deps_of_expr find_loc expr)

(* dereference of an lvalue: first, its address must be computed,
   then its contents themselves are read *)
and deps_of_lval find_loc lval =
  let ploc = find_loc lval in
  let zone = Precise_locs.enumerate_valid_bits Read ploc in
  { data = zone;
    indirect = indirect_zone_of_lval find_loc lval; }

(* Computations of the inputs of a lvalue : union of the "host" part and
   the offset. *)
and indirect_zone_of_lval find_loc (lhost, offset) =
  Locations.Zone.join
    (zone_of_lhost find_loc lhost) (zone_of_offset find_loc offset)

(* Computation of the inputs of a host. Nothing for a variable, and the
   inputs of [e] for a dereference [*e]. *)
and zone_of_lhost find_loc = function
  | Var _ -> Locations.Zone.bottom
  | Mem e -> zone_of_expr find_loc e

(* Computation of the inputs of an offset. *)
and zone_of_offset find_loc = function
  | NoOffset -> Locations.Zone.bottom
  | Field (_, o) -> zone_of_offset find_loc o
  | Index (e, o) ->
    Locations.Zone.join
      (zone_of_expr find_loc e) (zone_of_offset find_loc o)

let rec height_expr expr =
  match expr.enode with
  | Const _ | SizeOf _ | SizeOfStr _ | AlignOf _ -> 0
  | Lval lv | AddrOf lv | StartOf lv  -> height_lval lv + 1
  | UnOp (_,e,_) | CastE (_, e) | SizeOfE e | AlignOfE e
    -> height_expr e + 1
  | BinOp (_,e1,e2,_) -> max (height_expr e1) (height_expr e2) + 1

and height_lval (host, offset) =
  let h1 = match host with
    | Var _ -> 0
    | Mem e -> height_expr e + 1
  in
  max h1 (height_offset offset) + 1

and height_offset = function
  | NoOffset  -> 0
  | Field (_,r) -> height_offset r + 1
  | Index (e,r) -> max (height_expr e) (height_offset r) + 1


let skip_specifications kf =
  Parameters.SkipLibcSpecs.get () &&
  Kernel_function.is_definition kf &&
  Cil.is_in_libc (Kernel_function.get_vi kf).vattr

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
