(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's E-ACSL plug-in.                    *)
(*                                                                        *)
(*  Copyright (C) 2012-2024                                               *)
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

(** E-ACSL. *)

open Cil_types

module Options: sig
  type category
end

module Error: sig
  exception Typing_error of Options.category option * string
  exception Not_yet of Options.category option * string
end

module Translate_terms: sig
  exception No_simple_translation of term
  val untyped_to_exp: typ option -> term -> exp
  (** @raise Typing_error when the given term cannot be typed (something wrong
      happened with this term)
      @raise Not_yet when the given term contains an unsupported construct.
      @raise No_simple_translation when the given term cannot be translated
      into a single expression. *)
end

module Translate_predicates: sig
  exception No_simple_translation of predicate
  val untyped_to_exp: predicate -> exp
  (** @raise Typing_error when the given predicate cannot be typed
      (something wrong happened with this predicate).
      @raise Not_yet when the given predicate contains an unsupported construct.
      @raise No_simple_translation when the given predicate cannot be
      translated into a single expression. *)
end

module Functions: sig
  module RTL: sig
    val is_generated_name: string -> bool
    (** @return [true] if the prefix of the given name indicates that it has
        been generated by E-ACSL instrumentation (see [mk_gen_name] function).
    *)
  end
end

(** No function is directly exported: they are dynamically registered. *)

(*
Local Variables:
compile-command: "make"
End:
*)
