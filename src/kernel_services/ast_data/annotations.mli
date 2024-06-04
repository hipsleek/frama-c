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

(** Annotations in the AST.
    The AST should be computed before calling functions of this module.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

open Cil_types

(**************************************************************************)
(** {2 Getting annotations} *)
(**************************************************************************)

val add_hook_on_change: (unit -> unit) -> unit
(** Emitted whenever any of the annotation tables is modified.
    @since 27.0-Cobalt *)

(**************************************************************************)
(** {3 Code annotations} *)
(**************************************************************************)

val code_annot:
  ?emitter:Emitter.t ->
  ?filter:(code_annotation -> bool) ->
  stmt ->
  code_annotation list
(** Get all the code annotations attached to the given statement.
    If [emitter] (resp. [filter]) is specified, return only the annotations that
    has been generated by this [emitter] (resp. that satisfies the given
    predicate). *)

val code_annot_emitter:
  ?filter:(Emitter.t -> code_annotation -> bool) ->
  stmt ->
  (code_annotation * Emitter.t) list
(** Same as {!code_annot}, but also returns the emitter who emitted the
    annotation.
    @since Fluorine-20130401 *)

(**************************************************************************)
(** {3 Function Contracts} *)
(**************************************************************************)

exception No_funspec of Emitter.t
val funspec:
  ?emitter:Emitter.t -> kernel_function -> funspec
(** Get the contract associated to the given function.
    If [emitter] is specified, return only the annotations that
    has been generated by this [emitter].
    @raise No_funspec whenever the given function has no specification
    @before 28.0-Nickel the optional parameter [populate] was meant to generate
    missing spec. Use {!Populate_spec.populate_funspec} instead. *)

val has_funspec: kernel_function -> bool
(** @return [true] iff the function has a non-empty specification.
    @since 22.0-Titanium *)

val behaviors:
  ?emitter:Emitter.t -> kernel_function -> funbehavior list
(** Get the behaviors clause of the contract associated to the given function.
    Meaning of [emitter] and [populate] is similar to {!funspec}.
    @raise No_funspec whenever the given function has no specification
    @before 28.0-Nickel the optional parameter [populate] was meant to generate
    missing spec. Use {!Populate_spec.populate_funspec} instead. *)

val decreases:
  ?emitter:Emitter.t -> kernel_function -> variant option
(** If any, get the decrease clause of the contract associated to the given
    function. Meaning of [emitter] is similar to {!funspec}.
    @raise No_funspec whenever the given function has no specification
    @before 28.0-Nickel the optional parameter [populate] was meant to generate
    missing spec. Use {!Populate_spec.populate_funspec} instead. *)

val terminates:
  ?emitter:Emitter.t -> kernel_function -> identified_predicate option
(** If any, get the terminates clause of the contract associated to the given
    function. Meaning of [emitter] is similar to {!funspec}.
    @raise No_funspec whenever the given function has no specification
    @before 28.0-Nickel the optional parameter [populate] was meant to generate
    missing spec. Use {!Populate_spec.populate_funspec} instead. *)

val complete:
  ?emitter:Emitter.t -> kernel_function -> string list list
(** Get the complete behaviors clause of the contract associated to the given
    function. Meaning of [emitter] is similar to {!funspec}.
    @raise No_funspec whenever the given function has no specification
    @before 28.0-Nickel the optional parameter [populate] was meant to generate
    missing spec. Use {!Populate_spec.populate_funspec} instead. *)

val disjoint:
  ?emitter:Emitter.t -> kernel_function -> string list list
(** If any, get the disjoint behavior clause of the contract associated to the
    given function. Meaning of [emitter] is similar to {!funspec}.
    @raise No_funspec whenever the given function has no specification
    @before 28.0-Nickel the optional parameter [populate] was meant to generate
    missing spec. Use {!Populate_spec.populate_funspec} instead. *)

(**************************************************************************)
(** {3 Global Annotations} *)
(**************************************************************************)

val model_fields: ?emitter:Emitter.t -> typ -> model_info list
(** returns the model fields attached to a given type (either directly or
    because the type is a typedef of something that has model fields.
    @since Fluorine-20130401
*)

(**************************************************************************)
(** {2 Iterating over annotations} *)
(**************************************************************************)

val iter_code_annot:
  (Emitter.t -> code_annotation -> unit) -> stmt -> unit
(** Iter on each code annotation attached to the given statement. *)

val fold_code_annot:
  (Emitter.t -> code_annotation -> 'a -> 'a) -> stmt -> 'a -> 'a
(** Fold on each code annotation attached to the given statement. *)

val iter_all_code_annot:
  ?sorted:bool ->
  (stmt -> Emitter.t -> code_annotation -> unit) -> unit
(** Iter on each code annotation of the program.
    If [sorted] is [true] (the default), iteration is sorted according
    to the location of the statements and by emitter. Note that the
    sorted version is less efficient than the unsorted iteration.
*)

val fold_all_code_annot:
  ?sorted:bool ->
  (stmt -> Emitter.t -> code_annotation -> 'a -> 'a) -> 'a -> 'a
(** Fold on each code annotation of the program. See above for
    the meaning of the [sorted] argument.
*)

val iter_global:
  (Emitter.t -> global_annotation -> unit) -> unit
(** Iter on each global annotation of the program. *)

val fold_global:
  (Emitter.t -> global_annotation -> 'a -> 'a) -> 'a -> 'a
(** Fold on each global annotation of the program. *)

val iter_requires:
  (Emitter.t -> identified_predicate -> unit) ->
  kernel_function -> string -> unit
(** Iter on the requires of the corresponding behavior.
    @since Fluorine-20130401 *)

val fold_requires:
  (Emitter.t -> identified_predicate -> 'a -> 'a) ->
  kernel_function -> string -> 'a -> 'a
(** Fold on the requires of the corresponding behavior. *)

val iter_assumes:
  (Emitter.t -> identified_predicate -> unit) ->
  kernel_function -> string -> unit
(** Iter on the assumes of the corresponding behavior.
    @since Fluorine-20130401 *)

val fold_assumes:
  (Emitter.t -> identified_predicate -> 'a -> 'a) ->
  kernel_function -> string -> 'a -> 'a
(** Fold on the assumes of the corresponding behavior. *)

val iter_ensures:
  (Emitter.t -> (termination_kind * identified_predicate) -> unit) ->
  kernel_function -> string -> unit
(** Iter on the ensures of the corresponding behavior.
    @since Fluorine-20130401 *)

val fold_ensures:
  (Emitter.t -> (termination_kind * identified_predicate) -> 'a -> 'a) ->
  kernel_function -> string -> 'a -> 'a
(** Fold on the ensures of the corresponding behavior. *)

val iter_assigns:
  (Emitter.t -> assigns -> unit) ->
  kernel_function -> string -> unit
(** Iter on the assigns of the corresponding behavior.
    @since Fluorine-20130401 *)

val fold_assigns:
  (Emitter.t -> assigns -> 'a -> 'a) ->
  kernel_function -> string -> 'a -> 'a
(** Fold on the assigns of the corresponding behavior. *)

val iter_allocates:
  (Emitter.t -> allocation -> unit) ->
  kernel_function -> string -> unit
(** Iter on the allocates of the corresponding behavior.
    @since Fluorine-20130401 *)

val fold_allocates:
  (Emitter.t -> allocation -> 'a -> 'a) ->
  kernel_function -> string -> 'a -> 'a
(** Fold on the allocates of the corresponding behavior. *)

val iter_extended:
  (Emitter.t -> acsl_extension -> unit) ->
  kernel_function -> string -> unit
(** @since Sodium-20150201 *)

val fold_extended:
  (Emitter.t -> acsl_extension -> 'a -> 'a) ->
  kernel_function -> string -> 'a -> 'a

val iter_behaviors:
  (funbehavior -> unit) -> kernel_function -> unit
(** Iter on the behaviors of the given kernel function.
    @since Fluorine-20130401
    @before 29.0-Copper previous version was behaving as
    {!iter_behaviors_by_emitter} *)

val fold_behaviors:
  (funbehavior -> 'a -> 'a) -> kernel_function -> 'a -> 'a
(** Fold on the behaviors of the given kernel function.
    @before 29.0-Copper previous version was behaving as
    {!fold_behaviors_by_emitter} *)

val iter_behaviors_by_emitter:
  (Emitter.t -> funbehavior -> unit) -> kernel_function -> unit
(** Iter on the behaviors, for each emitter, of the given kernel function.
    @since 29.0-Copper *)

val fold_behaviors_by_emitter:
  (Emitter.t -> funbehavior -> 'a -> 'a) -> kernel_function -> 'a -> 'a
(** Fold on the behaviors, for each emitter, of the given kernel function.
    @since 29.0-Copper *)

val iter_complete:
  (Emitter.t -> string list -> unit) -> kernel_function -> unit
(** Iter on the complete clauses of the given kernel function.
    @since Fluorine-20130401 *)

val fold_complete:
  (Emitter.t -> string list -> 'a -> 'a) -> kernel_function -> 'a -> 'a
(** Fold on the complete clauses of the given kernel function. *)

val iter_disjoint:
  (Emitter.t -> string list -> unit) -> kernel_function -> unit
(** Iter on the disjoint clauses of the given kernel function.
    @since Fluorine-20130401 *)

val fold_disjoint:
  (Emitter.t -> string list -> 'a -> 'a) -> kernel_function -> 'a -> 'a
(** Fold on the disjoint clauses of the given kernel function. *)

val iter_terminates:
  (Emitter.t -> identified_predicate -> unit) -> kernel_function -> unit
(** apply f to the terminates predicate if any.
    @since Fluorine-20130401 *)

val fold_terminates:
  (Emitter.t -> identified_predicate -> 'a -> 'a) -> kernel_function -> 'a -> 'a
(** apply f to the terminates predicate if any. *)

val iter_decreases:
  (Emitter.t -> variant -> unit) -> kernel_function -> unit
(** apply f to the decreases term if any.
    @since Fluorine-20130401 *)

val fold_decreases:
  (Emitter.t -> variant -> 'a -> 'a) -> kernel_function -> 'a -> 'a
(** apply f to the decreases term if any. *)

(**************************************************************************)
(** {2 Adding annotations} *)
(**************************************************************************)

val add_code_annot:
  ?keep_empty:bool ->
  Emitter.t -> ?kf:kernel_function -> stmt -> code_annotation -> unit
(** Add a new code annotation attached to the given statement. If [kf] is
    provided, the function runs faster.

    There might be at most one statement contract associated to
    a given statement and a given set of enclosing behaviors.
    Trying to associate more than one will result in a merge of the contracts.

    The same things happens with loop assigns and allocates/frees.
    The [keep_empty] argument is only used for loop assigns
    and loop allocates, where it is used to decide whether to add the given
    code annot in case the corresponding category was empty. It defaults to
    [true], which is sound wrt ACSL semantics of equating an absence of
    annotation with assigns/allocates \everything.

    There can be at most one loop variant registered per statement.
    Attempting to register a second one will result in a fatal error.

    @before 22.0-Titanium there was no [keep_empty] optional argument
*)

val add_assert:
  Emitter.t -> ?kf:kernel_function -> stmt -> predicate -> unit
(** Add an assertion attached to the given statement. If [kf] is
    provided, the function runs faster.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val add_check:
  Emitter.t -> ?kf:kernel_function -> stmt -> predicate -> unit
(** Add a checking assertion attached to the given statement. If [kf] is
    provided, the function runs faster.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val add_admit:
  Emitter.t -> ?kf:kernel_function -> stmt -> predicate -> unit
(** Add an hypothesis assertion attached to the given statement. If [kf] is
    provided, the function runs faster.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val add_global: Emitter.t -> global_annotation -> unit
(** Add a new global annotation into the program. *)

type 'a contract_component_addition =
  Emitter.t ->
  kernel_function -> ?stmt:stmt -> ?active:string list -> 'a -> unit
(** type for functions adding a part of a contract (either for global function
    or for a given [stmt]). In the latter case [active] may be used to state the
    list of enclosing behavior(s) for which the contract is supposed to hold.
    [active] defaults to the empty list, meaning that the contract must
    always hold.

    @since Aluminium-20160501
*)

type 'a behavior_component_addition =
  Emitter.t ->
  kernel_function -> ?stmt:stmt -> ?active:string list ->
  ?behavior:string -> 'a -> unit
(** type for functions adding a part of a [behavior] inside a contract. The
    contract is found in a similar way as for {!contract_component_addition}
    functions. Similarly, [active] has the same meaning as for
    {!contract_component_addition}.

    Default for [behavior] is {!Cil.default_behavior_name}.

    @since Aluminium-20160501
*)

exception AlreadySpecified of string list
(** raised when a specification can't be added since there is already one, the
    list contains the clause kind that can't be addeed (e.g: "decreases"). *)

val add_spec:
  ?register_children:bool -> ?force:bool -> spec contract_component_addition
(** Add new spec into the given contract. The [force] (which defaults to
    [false]) parameter is used to determine whether [decreases] and [terminates]
    clauses must be replaced if they already exist and a new one is provided.

    More precisely, if [force] is [true] *and* the new contract has
    [Some terminates], the old one is removed and the new clause is used
    (the same applies for [decreases]). *But* if the new clause is [None], the
    old one is kept. If you really want to remove some of these clauses, use
    {!remove_decreases} and {!remove_terminates}.

    If [force] is [false] and the contract has [Some terminates] (or decreases)
    and the old contract already has such specification, an exception
    [AlreadySpecified] is raised. Note that in this case, the function does not
    perform any modification to the spec.

    [register_children] is directly given to the function [add_behaviors].

    @since 23.0-Vanadium
    @before 26.0-Iron: the [force] parameter does not exist
*)

val add_behaviors:
  ?register_children:bool -> funbehavior list contract_component_addition
(** Add new behaviors into the given contract.
    if [register_children] is [true] (the default), inner clauses of the
    behavior will also be registered by the function.
*)

val add_decreases:
  ?force:bool -> Emitter.t -> kernel_function -> variant -> unit
(** Add a decrease clause into the contract of the given function.

    If [force] is [false] (default), if a clause is already attached to the
    function, an exception [AlreadySpecified] is raised. If [force] is [true]
    the old specification is dropped and the new one replaces it.

    @before 26.0-Iron: the [force] parameter does not exist
*)

val add_terminates:
  ?force:bool -> identified_predicate contract_component_addition
(** Add a terminates clause into the contract of the given function.

    If [force] is [false] (default), if a clause is already attached to the
    function, an exception [AlreadySpecified] is raised. If [force] is [true]
    the old specification is dropped and the new one replaces it.

    @before 26.0-Iron: the [force] parameter does not exist
*)

val add_complete: string list contract_component_addition
(** Add a new complete behaviors clause into the contract of the given
    function. Do nothing (but emit a warning) if this clause already exists
    in the spec.

    @raise Fatal if one of the given name
    is either an unknown behavior or {!Cil.default_behavior_name}.
*)

val add_disjoint: string list contract_component_addition
(** Add a new disjoint behaviors clause into the contract of the given
    function. Do nothing (but emit a warning) if this clause already exists
    in the spec.

    @raise Fatal if one of the given name
    is either an unknown behavior or {!Cil.default_behavior_name}.
*)

val add_requires: identified_predicate list behavior_component_addition
(** Add new requires clauses into the given behavior.
*)

val add_assumes: identified_predicate list behavior_component_addition
(** Add new assumes clauses into the given behavior.

    Does nothing but emitting a warning if an attempt is
    made to add assumes clauses to the default behavior.
*)

val add_ensures:
  (termination_kind * identified_predicate) list behavior_component_addition
(** Add new ensures clauses into the given behavior.
*)

val add_assigns:
  keep_empty:bool -> assigns behavior_component_addition
(** Add new assigns into the given behavior.

    If [keep_empty] is [true] and the assigns clause were empty, then
    the assigns clause remains empty. (That corresponds to the ACSL semantics of
    an assigns clause: if no assigns is specified, that is equivalent to assigns
    everything.)
*)

val add_allocates:
  keep_empty:bool -> allocation behavior_component_addition
(** Add new allocates into the given behavior.
    See {!Annotations.add_assigns} for the signification of [keep_empty]
    @before 22.0-Titanium there is no [keep_empty] argument
*)

val add_extended: acsl_extension behavior_component_addition
(** @since Sodium-20150201 *)

(**************************************************************************)
(** {2 Removing annotations} *)
(**************************************************************************)

val remove_code_annot:
  Emitter.t -> ?kf:kernel_function -> stmt -> code_annotation -> unit
(** Remove a code annotation attached to a statement. The provided emitter must
    be the one that emits this annotation. Do nothing if the annotation does not
    exist, or if the emitter is not ok. *)

val remove_global: Emitter.t -> global_annotation -> unit
(** Remove a global annotation. The provided emitter must be the one that emits
    this annotation. Do nothing if the annotation does not exist, or if the
    emitter is not ok. It is the responsibility of the user to ensure that
    logic functions/predicates declared in the given annotation are not used
    elsewhere.
*)

val remove_behavior:
  ?force:bool -> Emitter.t -> kernel_function -> funbehavior -> unit
(** Remove a behavior attached to a function. The provided emitter must be the
    one that emits this annotation. Do nothing if the annotation does not exist,
    or if the emitter is not ok. If [force] is [false] (which is the default),
    it is not possible to remove a behavior whose the name is used in a
    complete/disjoint clause. If [force] is [true], it is the responsibility
    of the user to ensure that complete/disjoint clauses refer to existing
    behaviors.
*)

val remove_behavior_components:
  Emitter.t -> kernel_function -> funbehavior -> unit
(** remove all the component of a behavior, but keeps the name (so as to
    avoid issues with disjoint/complete clauses). *)

val remove_decreases: Emitter.t -> kernel_function -> unit
(** Remove the decreases clause attached to a function. The provided emitter
    must be the one that emits this annotation. Do nothing if the annotation
    does not exist, or if the emitter is not ok. *)

val remove_terminates: Emitter.t -> kernel_function -> unit
(** Remove the terminates clause attached to a function. The provided emitter
    must be the one that emits this annotation. Do nothing if the annotation
    does not exist, or if the emitter is not ok. *)

val remove_complete: Emitter.t -> kernel_function -> string list -> unit
(** Remove a complete behaviors clause attached to a function. The provided
    emitter must be the one that emits this annotation. Do nothing if the
    annotation does not exist, or if the emitter is not ok. *)

val remove_disjoint: Emitter.t -> kernel_function -> string list -> unit
(** Remove a disjoint behaviors clause attached to a function. The provided
    emitter must be the one that emits this annotation. Do nothing if the
    annotation does not exist, or if the emitter is not ok. *)

val remove_requires:
  Emitter.t -> kernel_function -> identified_predicate -> unit
(** Remove a requires clause from the spec of the given function. Do nothing
    if the predicate does not exist or
    was not emitted by the given emitter. *)

val remove_assumes:
  Emitter.t -> kernel_function -> identified_predicate -> unit
(** Remove an assumes clause from the spec of the given function. Do nothing
    if the predicate does not exist or was not emitted
    by the given emitter. *)

val remove_ensures:
  Emitter.t -> kernel_function ->
  (termination_kind * identified_predicate) -> unit
(** Remove a post-condition from the spec of the given function. Do nothing
    if the post-cond does not exist or was not emitted
    by the given emitter. *)

val remove_allocates:
  Emitter.t -> kernel_function -> allocation -> unit
(** Remove the corresponding allocation clause. Do nothing if the clause
    does not exist or was not emitted by the given emitter. *)

val remove_assigns:
  Emitter.t -> kernel_function -> assigns -> unit
(** Remove the corresponding assigns clause. Do nothing if the clause
    does not exist or was not emitted by the given emitter. *)

val remove_extended: Emitter.t -> kernel_function -> acsl_extension -> unit
(** @since Sodium-20150201 *)

(**************************************************************************)
(** {2 Other useful functions} *)
(**************************************************************************)

val has_code_annot: ?emitter:Emitter.t -> stmt -> bool
(** @return [true] iff there is some annotation attached to the given statement
    (and generated by the given emitter, if any). *)

val emitter_of_code_annot: code_annotation -> stmt -> Emitter.t
(** @return the emitter which generated the given code_annotation,
    assumed to be registered at the given statement.
    @raise Not_found if the code annotation does not exist, or if it is
    registered at another statement.

    @since Magnesium-20151001
*)

val emitter_of_global: global_annotation -> Emitter.t
(** @return the emitter which generates a global annotation.
    @raise Not_found if the global annotation is not registered. *)

val logic_info_of_global: string -> logic_info list
(** @return the purely logic var of the given name
    @raise Not_found if no global annotation declare such a variable *)

val behavior_names_of_stmt_in_kf: kernel_function -> string list
(** @return all the behavior names included in any statement contract of the
    given function. *)

val code_annot_of_kf: kernel_function -> (stmt * code_annotation) list
(** @return all the annotations attached to a statement of the given
    function. *)

val fresh_behavior_name: kernel_function -> string -> string
(** @return a valid behavior name for the given function and based on the given
    name. *)

(**************************************************************************)
(** {2 States} *)
(**************************************************************************)

val code_annot_state: State.t
(** The state which stores all the code annotations of the program. *)

val funspec_state: State.t
(** The state which stores all the function contracts of the program. *)

val global_state: State.t
(** The state which stores all the global annotations of the program. *)

(**/**)

(**************************************************************************)
(** {2 Internal stuff} *)
(**************************************************************************)

val populate_spec_ref: (kernel_function -> funspec -> bool) ref
[@@ deprecated "Use Populate_spec.populate_funspec instead."]

val unsafe_add_global: Emitter.t -> global_annotation -> unit
val register_funspec:
  ?emitter:Emitter.t -> ?force:bool -> kernel_function -> unit
val remove_alarm_ref:
  (Emitter.Usable_emitter.t -> stmt -> code_annotation -> unit) ref

(*
  Local Variables:
  compile-command: "make -C ../../.."
  End:
*)