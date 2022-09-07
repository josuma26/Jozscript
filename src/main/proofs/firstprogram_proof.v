From Coq Require Import Lia.

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


Definition empty := fun {T: Type} => nil(tt).

Definition cons := fun {T: Type} => (fun x: T => (fun L: List T => cons((x, L)))).

Definition isEmpty := fun {T: Type} => (fun l: List T => match l with 
	| (Nil u) => true
	| (Cons (first, rest)) => false
	end).

Definition isCons := fun {T: Type} => (fun l: List T => match l with 
	| (Nil u) => false
	| (Cons (first, rest)) => true
	end).

Fixpoint singleton {T} (t: T): List T := 
	((cons t) empty).

Fixpoint append {T} (l1: List T)  (l2: List T): List T := 
	match l1 with 
	| (Nil u) => l2
	| (Cons (first, rest)) => ((cons first) ((append rest) l2))
	end.

Fixpoint contains {T} (list: List T)  (x: T)  (eq: (T -> (T -> bool))): bool := 
	match list with 
	| (Nil u) => false
	| (Cons (first, rest)) => (((eq first) x) || (((contains rest) x) eq))
	end.

Fixpoint remove {T} (list: List T)  (x: T)  (eq: (T -> (T -> bool))): List T := 
	match list with 
	| (Nil u) => list
	| (Cons (first, rest)) => match ((eq x) first) with 
	| true => (((remove rest) x) eq)
	| false => ((cons first) (((remove rest) x) eq))
	end
	end.

Fixpoint generateList {T} (n: nat)  (f: (nat -> T)): List T := 
	match n with 
	| 0 => empty
	| i => cons(((f i), ((generateList (i - 1)) f)))
	end.

Fixpoint len {T} (list: List T): nat := 
	match list with 
	| (Nil u) => 0
	| (Cons (first, rest)) => (1 + (len rest))
	end.

Fixpoint map {R T} (list: List T)  (func: (T -> R)): List R := 
	match list with 
	| (Nil u) => empty
	| (Cons (first, rest)) => ((cons (func first)) ((map rest) func))
	end.

Fixpoint foldr {R T} (list: List T)  (base: R)  (func: (T -> (R -> R))): R := 
	match list with 
	| (Nil u) => base
	| (Cons (first, rest)) => ((func first) (((foldr rest) base) func))
	end.

Fixpoint sumAll (list: List nat): nat := 
	(((foldr list) 0) (fun x: nat => (fun y: nat => (x + y)))).

Fixpoint forAll {T} (l: List T)  (op: (T -> unit)): unit := 
	(((foldr l) tt) (fun x: T => (fun u: unit => (op x)))).

Fixpoint addLast {T} (l: List T)  (x: T): List T := 
	match l with 
	| (Nil u) => (singleton x)
	| (Cons (first, rest)) => ((cons first) ((addLast rest) x))
	end.

Fixpoint isNotEmpty {T} (l: List T): bool := 
	match l with 
	| (Nil u) => false
	| (Cons (first, rest)) => true
	end.

(*
{{True}}

PatternMatch(cons[Nat](2)(empty[Nat]),List(((cons [first, rest]),PatternMatch(first,List((0,let x := 2), (i,let x := 3))))))

{{(x = 3)}}
*)

Theorem correct: forall (first: nat), forall (rest: List nat), ((((True) /\ (((True) /\ ((True) /\ (True))) /\ (((cons 2) empty) = cons((first, rest))))) /\ (0 = first)) -> ((2 = 3))) /\ ((((True) /\ (((True) /\ ((True) /\ (True))) /\ (((cons 2) empty) = cons((first, rest))))) /\ (i = first)) -> ((3 = 3))).
	Proof.
Admitted.