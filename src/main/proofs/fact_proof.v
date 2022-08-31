From Coq Require Import Lia.

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

Fixpoint factorial (n: nat): nat := 
	match n with 
	| 0 => 1
	| i => (i * (factorial (i - 1)))
	end.

(*
{{X: nat}}

let a := 1;
let x := X;
while ((x > 0)) {
	let a := (x * a);
let x := (x - 1)}

{{(a = (factorial X))}}
*)

Theorem correct: forall (X: nat), (True) /\ ((True) /\ ((((((True) /\ (a = 1)) /\ (x = X)) -> (((a * (factorial x)) = (factorial X)))) /\ ((True) /\ ((((((a' * (factorial x)) = (factorial X))) /\ ((x > 0))) /\ (a = (x * a'))) -> (((a * (factorial (x - 1))) = (factorial X)))))) /\ (((((a * (factorial x)) = (factorial X))) /\ (~(x > 0))) -> ((a = (factorial X)))))).
	Proof.
Admitted.