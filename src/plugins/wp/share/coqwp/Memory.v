(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2024                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(* This file is generated by Why3's Coq-realize driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require HighOrd.
Require bool.Bool.
Require int.Int.
Require map.Map.

Require Import ZArith.
Require Import Qedlib.

(* Why3 assumption *)
Inductive addr :=
  | mk_addr : Z -> Z -> addr.
Axiom addr_WhyType : WhyType addr.
Existing Instance addr_WhyType.

(* Why3 assumption *)
Definition offset (v:addr) : Z := match v with
                                  | mk_addr x x1 => x1
                                  end.

(* Why3 assumption *)
Definition base (v:addr) : Z := match v with
                                | mk_addr x x1 => x
                                end.

(* Why3 goal *)
Definition addr_le : addr -> addr -> Prop.
  exact (fun (p q : addr) => ((base p = base q) /\ (offset p <= offset q)%Z)).
Defined.

(* Why3 goal *)
Definition addr_lt : addr -> addr -> Prop.
  exact (fun (p q : addr) => (base p = base q) /\ (offset p < offset q)%Z).
Defined.

(* Why3 goal *)
Definition addr_le_bool : addr -> addr -> bool.
  exact (fun (p q : addr) =>
           andb (Zeq_bool (base p) (base q)) (Zle_bool (offset p) (offset q))).
Defined.

(* Why3 goal *)
Definition addr_lt_bool : addr -> addr -> bool.
  exact (fun (p q : addr) =>
           andb (Zeq_bool (base p) (base q)) (Zlt_bool (offset p) (offset q))).
Defined.

(* Why3 goal *)
Lemma addr_le_def :
  forall (p:addr) (q:addr), ((base p) = (base q)) ->
  (addr_le p q) <-> ((offset p) <= (offset q))%Z.
Proof.
  unfold addr_le.
  intuition.
Qed.

(* Why3 goal *)
Lemma addr_lt_def :
  forall (p:addr) (q:addr), ((base p) = (base q)) ->
  (addr_lt p q) <-> ((offset p) < (offset q))%Z.
Proof.
  unfold addr_lt.
  intuition.
Qed.

(* Why3 goal *)
Lemma addr_le_bool_def :
  forall (p:addr) (q:addr), (addr_le p q) <-> ((addr_le_bool p q) = true).
Proof.
  unfold addr_le. unfold addr_le_bool.
  intros. split; intro H.
  destruct H as [H0 H1].
  rewrite Zeq_is_eq_bool in H0.
  apply Zle_imp_le_bool in H1.
  rewrite H0. rewrite H1.
  compute;reflexivity.
  symmetry in H.
  apply Bool.andb_true_eq in H.
  destruct H as [H1 H2].
  split;[apply Zeq_bool_eq|apply Zle_bool_imp_le];symmetry; assumption.
Qed.

(* Why3 goal *)
Lemma addr_lt_bool_def :
  forall (p:addr) (q:addr), (addr_lt p q) <-> ((addr_lt_bool p q) = true).
Proof.
  unfold addr_lt. unfold addr_lt_bool.
  intros. split; intro H.
  destruct H as [H0 H1].
  rewrite Zeq_is_eq_bool in H0.
  rewrite Zlt_is_lt_bool in H1.
  rewrite H0. rewrite H1.
  compute;reflexivity.
  symmetry in H.
  apply Bool.andb_true_eq in H.
  destruct H as [H1 H2].
  split;[apply Zeq_bool_eq|rewrite Zlt_is_lt_bool];symmetry; assumption.
Qed.

(* Why3 assumption *)
Definition null : addr := mk_addr 0%Z 0%Z.

(* Why3 assumption *)
Definition global (b:Z) : addr := mk_addr b 0%Z.

(* Why3 assumption *)
Definition shift (p:addr) (k:Z) : addr :=
  mk_addr (base p) ((offset p) + k)%Z.

(* Why3 assumption *)
Definition included (p:addr) (a:Z) (q:addr) (b:Z) : Prop :=
  (0%Z < a)%Z ->
  (0%Z <= b)%Z /\
  (((base p) = (base q)) /\
   (((offset q) <= (offset p))%Z /\
    (((offset p) + a)%Z <= ((offset q) + b)%Z)%Z)).

(* Why3 assumption *)
Definition separated (p:addr) (a:Z) (q:addr) (b:Z) : Prop :=
  (a <= 0%Z)%Z \/
  ((b <= 0%Z)%Z \/
   (~ ((base p) = (base q)) \/
    ((((offset q) + b)%Z <= (offset p))%Z \/
     (((offset p) + a)%Z <= (offset q))%Z))).

(* Why3 assumption *)
Definition eqmem {a:Type} {a_WT:WhyType a} (m1: farray addr a) (m2:farray addr a)
    (p:addr) (a1:Z) : Prop :=
  forall (q:addr), (included q 1%Z p a1) -> ((m1 .[ q ]) = (m2 .[ q ])).

(* Why3 goal *)
Variable havoc: forall {a:Type} {a_WT:WhyType a}, (map.Map.map addr a) ->
  (map.Map.map addr a) -> addr -> Z -> map.Map.map addr a.

Definition fhavoc {A : Type}
  (m : farray addr A)
  (w : farray addr A) (p:addr) (n:Z) : (farray addr A) :=
  {| whytype1 := whytype1 m;
     whytype2 := whytype2 m;
     access := @havoc _ (whytype2 m) (access m) (access w) p n |}.

(* Why3 assumption *)
Definition valid_rw (m:array Z) (p:addr) (n:Z) : Prop :=
  (0%Z < n)%Z ->
  (0%Z < (base p))%Z /\
  ((0%Z <= (offset p))%Z /\ (((offset p) + n)%Z <= (m .[ base p ]))%Z).

(* Why3 assumption *)
Definition valid_rd (m:array Z) (p:addr) (n:Z) : Prop :=
  (0%Z < n)%Z ->
  ~ (0%Z = (base p)) /\
  ((0%Z <= (offset p))%Z /\ (((offset p) + n)%Z <= (m .[ base p ]))%Z).

(* Why3 assumption *)
Definition invalid (m:array Z) (p:addr) (n:Z) : Prop :=
  (0%Z < n)%Z ->
  ((m .[ base p ]) <= (offset p))%Z \/ (((offset p) + n)%Z <= 0%Z)%Z.

(* Why3 goal *)
Lemma valid_rw_rd :
  forall (m:array Z), forall (p:addr), forall (n:Z), (valid_rw m p n) ->
  valid_rd m p n.
Proof.
  intros m p n.
  unfold valid_rw. unfold valid_rd.
  intuition (auto with zarith).
Qed.

(* Why3 goal *)
Lemma valid_string :
  forall (m:array Z), forall (p:addr), ((base p) < 0%Z)%Z ->
  ((0%Z <= (offset p))%Z /\ ((offset p) < (m .[ base p ]))%Z) ->
  (valid_rd m p 1%Z) /\ ~ (valid_rw m p 1%Z).
Proof.
  intros m p.
  unfold valid_rd. unfold valid_rw.
  intuition (auto with zarith).
Qed.

Lemma separated_neq : forall p a q b p' q',
  separated p a q b ->
  included p' 1 p a ->
  included q' 1 q b ->
  p' <> q'.
Proof.
  intros p a q b p' q' SEP InP InQ EQ.
  unfold separated in SEP.
  unfold included in InP,InQ.
  case_lt 0%Z a.
  case_lt 0%Z b.
  intros BPOS APOS.
  generalize InP ; clear InP.
  intro H ; elim H ; clear H ; auto with zarith.
  intro H. clear H.
  intro H ; elim H ; clear H.
  intro BaseP.
  intro H ; elim H ; clear H.
  intros InP1 InP2.
  generalize InQ ; clear InQ.
  intro H ; elim H ; clear H ; auto with zarith.
  intro H. clear H.
  intro H ; elim H ; clear H.
  intro BaseQ.
  intro H ; elim H ; clear H.
  intros InQ1 InQ2.
  generalize SEP ; clear SEP.
  intro H ; elim H ; clear H ; auto with zarith.
  intro H ; elim H ; clear H ; auto with zarith.
  intro H ; elim H ; clear H ; auto with zarith.
  rewrite <- EQ in BaseQ.
  rewrite BaseP in BaseQ.
  contradiction.
  rewrite <- EQ in InQ1,InQ2.
  omega.
Qed.

(* Why3 goal *)
Lemma separated_1 :
  forall (p:addr) (q:addr), forall (a:Z) (b:Z) (i:Z) (j:Z),
  (separated p a q b) ->
  (((offset p) <= i)%Z /\ (i < ((offset p) + a)%Z)%Z) ->
  (((offset q) <= j)%Z /\ (j < ((offset q) + b)%Z)%Z) ->
  ~ ((mk_addr (base p) i) = (mk_addr (base q) j)).
Admitted.

(* Why3 goal *)
Definition region : Z -> Z.
Admitted.

(* Why3 goal *)
Definition linked : array Z -> Prop.
Admitted.

(* Why3 goal *)
Definition sconst : (farray addr Z) -> Prop.
Admitted.

(* Why3 assumption *)
Definition framed (m: farray addr addr) : Prop :=
  forall (p:addr), ((region (base (m .[ p ])) ) <= 0%Z)%Z.

(* Why3 goal *)
Lemma separated_included :
  forall (p:addr) (q:addr), forall (a:Z) (b:Z), (0%Z < a)%Z -> (0%Z < b)%Z ->
  (separated p a q b) -> ~ (included p a q b).
Proof.
intros p q a b h1 h2 h3.
  unfold separated. unfold included. unfold not.
  intuition.
Admitted.

(*
Lemma separated_region : forall p a q b,
  region (base p) <> region (base q) -> separated p a q b.
Proof.
  intros p a q b RDIFF.
  unfold separated.
  right. right. left.
  intuition.
  apply RDIFF. rewrite H. auto.
Qed.
*)

(* Why3 goal *)
Lemma included_trans :
  forall (p:addr) (q:addr) (r:addr), forall (a:Z) (b:Z) (c:Z),
  (included p a q b) -> (included q b r c) -> included p a r c.
Proof.
  intros p a q b r c.
  unfold included. intuition.
Qed.

(* Why3 goal *)
Lemma separated_trans :
  forall (p:addr) (q:addr) (r:addr), forall (a:Z) (b:Z) (c:Z),
  (included p a q b) -> (separated q b r c) -> separated p a r c.
Proof.
  intros p a q b r c.
Admitted.

(* Why3 goal *)
Lemma separated_sym :
  forall (p:addr) (q:addr), forall (a:Z) (b:Z),
  (separated p a q b) <-> (separated q b p a).
Proof.
  intros p q a b.
  unfold separated. intuition.
Qed.

(* Why3 goal *)
Lemma eqmem_included {a:Type} {a_WT:WhyType a} :
  forall (m1:farray addr a) (m2:farray addr a), forall (p:addr) (q:addr),
  forall (a1:Z) (b:Z), (included p a1 q b) -> (eqmem m1 m2 q b) ->
  eqmem m1 m2 p a1.
Proof.
  intros m1 m2 p q a1 b h1 h2.
Admitted.

(* Why3 goal *)
Lemma eqmem_sym {a:Type} {a_WT:WhyType a} :
  forall (m1:farray addr a) (m2: farray addr a), forall (p:addr), forall (a1:Z),
  (eqmem m1 m2 p a1) -> eqmem m2 m1 p a1.
Proof.
  intros m1 m2 p a1. unfold eqmem.
Admitted.

(* Why3 goal *)
Lemma havoc_access {a:Type} {a_WT:WhyType a} :
  forall (m0: farray addr a) (m1:farray addr a), forall (q:addr) (p:addr),
  forall (a1:Z),
  ((separated q 1%Z p a1) -> (((havoc m0 m1 p a1) q) = (m1 .[ q ]))) /\
  (~ (separated q 1%Z p a1) -> (((havoc m0 m1 p a1) q) = (m0 .[ q ]))).
Proof.
  intros m0 m1 q p a1.
Admitted.

(* Why3 goal *)
Definition int_of_addr : addr -> Z.
Admitted.

(* Why3 goal *)
Definition addr_of_int : Z -> addr.
Admitted.

Definition table : Type.
Admitted.

(* Why3 goal *)
Definition table_to_offset: table -> Z -> Z.
Admitted.

(* Why3 goal *)
Definition table_of_base: Z -> table.
Admitted.

(* Why3 goal *)
Lemma int_of_addr_bijection : forall (a:Z),
  ((int_of_addr (addr_of_int a)) = a).
Admitted.

(* Why3 goal *)
Lemma addr_of_int_bijection :
  forall (p:addr), ((addr_of_int (int_of_addr p)) = p).
Admitted.

(* Why3 goal *)
Lemma addr_of_null : ((int_of_addr null) = 0%Z).
Admitted.

(* Why3 goal *)
Lemma table_to_offset_zero : forall (t: table), ((table_to_offset t 0%Z) = 0%Z).
Admitted.

(* Why3 goal *)
Lemma table_to_offset_monotonic : forall (t:table) (i:Z) (j:Z), (i <= j)%Z ->
  ((table_to_offset t i) <= (table_to_offset t j))%Z.
Admitted.

Definition cinits (m: farray addr bool) : Prop.
Admitted.

Definition is_init_range(m: farray addr bool) (p: addr) (l: Z) :=
  forall i : int, (0 <= i)%Z /\ (i < l)%Z -> m .[ shift p i ] = true.

Definition set_init (m: farray addr bool) (p:addr) (a: Z) : farray addr bool.
Admitted.

Lemma set_init_access:
  forall m: farray addr bool,
  forall q p : addr,
  forall a : int,
    (~  separated q 1%Z p a -> (set_init m p a) .[ q ] = m .[ q ])
    /\ (separated q 1%Z p a -> (set_init m p a) .[ q ] = true).
Admitted.

Definition monotonic_init(m1 m2 : farray addr bool) :=
  forall p: addr, m1 .[ p ] = true -> m2 .[ p ] = true.