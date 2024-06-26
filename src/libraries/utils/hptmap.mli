(**************************************************************************)
(*                                                                        *)
(*  This file was originally part of Menhir                               *)
(*                                                                        *)
(*  François Pottier and Yann Régis-Gianas, INRIA Rocquencourt            *)
(*                                                                        *)
(*  Copyright 2005 Institut National de Recherche en Informatique et      *)
(*  en Automatique. All rights reserved. This file is distributed         *)
(*  under the terms of the Q Public License version 1.0, with the         *)
(*  change described in the file licenses/Q_MODIFIED_LICENSE.             *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(*                                                                        *)
(**************************************************************************)

(** Efficient maps from hash-consed trees to values, implemented as
    Patricia trees. *)

(** This implementation of big-endian Patricia trees follows Chris
    Okasaki's paper at the 1998 ML Workshop in Baltimore.  Maps are
    implemented on top of Patricia trees. A tree is big-endian if it
    expects the key's most significant bits to be tested first. *)

(** Undocumented. Needed for advanced users only *)
type prefix

(** Type of the keys of the map. *)
module type Id_Datatype = sig
  include Datatype.S
  val id: t -> int (** Identity of a key. Must verify [id k >= 0] and
                       [equal k1 k2 ==> id k1 = id k2] *)
end

(** Values stored in the map *)
module type V = sig
  include Datatype.S
  val pretty_debug: t Pretty_utils.formatter
end

(** This functor builds {!Hptmap_sig.Shape} for maps indexed by keys [Key],
    which contains all functions on hptmap that do not create or modify maps. *)
module Shape (Key : Id_Datatype): sig
  include Hptmap_sig.Shape with type key = Key.t
  type 'a t = 'a map
end

(** A boolean information is maintained for each tree, by composing the
    boolean on the subtrees and the value information present on each leaf.
    Use {!Comp_unused} for a default implementation. *)
module type Compositional_bool = sig
  type key
  type v

  val e : bool
  (** Value for the empty tree *)

  val f : key -> v -> bool
  (** Value for a leaf *)

  val compose : bool -> bool -> bool
  (** Composition of the values of two subtrees *)
end

module type Initial_values = sig
  type key
  type v
  val v : (key*v) list list
  (** List of the maps that must be shared between all instances of Frama-C
      (the maps being described by the list of their elements).
      Must include all maps that are exported at Caml link-time when the
      functor is applied. This usually includes at least the empty map, hence
      [v] nearly always contains [[]]. *)
end

module type Datatype_deps = sig
  val l : State.t list
  (** Dependencies of the hash-consing table. The table will be cleared
      whenever one of those dependencies is cleared. *)
end

(** This functor builds the complete module of maps indexed by keys [Key]
    to values [V]. *)
module Make
    (Key : Id_Datatype)
    (V : V)
    (_ : Compositional_bool with type key := Key.t
                             and type v := V.t)
    (_ : Initial_values with type key := Key.t
                         and type v := V.t)
    (_ : Datatype_deps)
  : Hptmap_sig.S with type key = Key.t
                  and type v = V.t
                  and type 'v map = 'v Shape(Key).map
                  and type prefix = prefix

(** Default implementation for the [Compositional_bool] argument of the functor
    {!Make}. To be used when no interesting compositional bit can be computed. *)
module Comp_unused : sig
  val e : bool
  val f : 'a -> 'b -> bool
  val compose : bool -> bool -> bool
end

(*
Local Variables:
compile-command: "make -C .."
End:
*)
