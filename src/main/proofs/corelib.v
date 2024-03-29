From Coq Require Import Lia.

Set Implicit Arguments.

Axiom orb_true_intro: forall (a b: bool),
    a = true \/ b = true -> (orb a  b) = true.

Axiom orb_prop: forall (a b: bool),
    (orb a b) = true -> a = true \/ b = true.

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

Inductive List (T: Type): Type :=
  | Nil: unit -> List T
  | Cons: (T * List T) -> List T.

Arguments Nil {_} _.

Definition list_ind :
  forall {T} (P: List T -> Prop),
    (forall (u: unit), P (Nil u)) ->
    (forall x l, P l -> P (Cons (x,l))) -> (forall l, P l) :=
  fun {T} => fun P => fun P1 => fun Pl =>
    fix f (l: List T):= match l with
                          | (Nil tt) => P1 tt
                          | (Cons (first, rest)) => Pl first rest (f rest)
                          end.
