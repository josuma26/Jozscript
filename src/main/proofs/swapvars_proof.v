From Coq Require Import Lia.

Axiom orb_true_intro: forall (a b: bool),
    a = true \/ b = true -> (orb a  b) = true.

Axiom orb_prop: forall (a b: bool),
    a = true \/ b = true -> (orb a b) = true.

Theorem minus0: forall n, n - 0 = n. Proof. induction n; auto.  Qed.

Theorem plus0: forall n, n + 0 = n. Proof. induction n; auto. Qed.

Theorem n_minus_n: forall n, n - n = 0. Proof. induction n; auto. Qed.

Theorem times1: forall n, n * 1 = n. Proof. induction n; auto. simpl. f_equal. auto. Qed.

Ltac rewrites := try rewrite minus0; try rewrite plus0; try rewrite n_minus_n; try rewrite times1; simpl; auto; try lia.

Ltac myauto :=
  intros; repeat (split; auto); intros;
  repeat match goal with
    | [ h: _ /\ _ |- _ ] => destruct h
         end;  subst; simpl; auto; rewrites.

Ltac isZero n := destruct n; [rewrites | lia].
(*
{{True}}

Definition a := 10.

Definition b := 5.

Definition a := (a + b).

Definition b := (a - b).

Definition a := (a - b).

{{((a = 5)) /\ ((b = 10))}}
*)

Theorem correct: forall (a: nat), forall (a': nat), forall (b: nat), forall (b': nat), (True) /\ ((True) /\ ((True) /\ ((True) /\ ((((((((((True) /\ (a' = 10)) /\ ((True) /\ (True))) /\ (b' = 5)) /\ ((True) /\ (True))) /\ (a = (a' + b'))) /\ ((True) /\ (True))) /\ (b = (a - b'))) /\ ((True) /\ (True))) -> ((((a - b) = 5)) /\ ((b = 10))))))).
	Proof.
Admitted.