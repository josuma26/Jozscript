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

Fixpoint fibonacci (n: nat): nat := 
	match n with 
	| 0 => 1
	| S i => match i with
             | 0 => 1
             | S i' => (fibonacci i) + (fibonacci i') end
	end.

(*
{{N: nat}}

let n := N;
let a := 1;
let b := 1;
while ((n > 0)) {
	let b := (a + b);
let a := (b - a);
let n := (n - 1)}

{{(a = (fibonacci N))}}
*)

Theorem correct: forall (N n a' a b b': nat), (True) /\ ((True) /\ ((True) /\ (((((((True) /\ (n = N)) /\ (a = 1)) /\ (b = 1)) -> (((a = (fibonacci (N - n)))) /\ ((b = (fibonacci ((N - n) + 1)))))) /\ ((True) /\ ((True) /\ (((((((a' = (fibonacci (N - n)))) /\ ((b' = (fibonacci ((N - n) + 1))))) /\ ((n > 0))) /\ (b = (a' + b'))) /\ (a = (b - a'))) -> (((a = (fibonacci (N - (n - 1))))) /\ ((b = (fibonacci ((N - (n - 1)) + 1))))))))) /\ (((((a = (fibonacci (N - n)))) /\ ((b = (fibonacci ((N - n) + 1))))) /\ (~(n > 0))) -> ((a = (fibonacci N))))))).
	Proof.
    myauto.
    - admit.
    - admit.
    - isZero n.
Admitted.
