From Coq Require Import Lia.

Set Implicit Arguments.


Ltac myauto :=
  intros; repeat (split; auto); intros;
  repeat match goal with
    | [ h: _ /\ _ |- _ ] => destruct h
         end;  subst; auto; simpl.

Theorem minus0: forall n, n - 0 = n. Proof. induction n; auto.  Qed.

Theorem plus0: forall n, n + 0 = n. Proof. induction n; auto. Qed.

Theorem n_minus_n: forall n, n - n = 0. Proof. induction n; auto. Qed.

Theorem times1: forall n, n * 1 = n. Proof. induction n; auto. simpl. f_equal. auto. Qed.

Inductive List (T: Type): Type :=
  | Nil: unit -> List T
  | Cons: (T * List T) -> List T.

Arguments Nil {_} _.

Definition isEmpty := fun {T: Type} => (fun l: List T => match l with 
	| (Nil u) => true
	| (Cons (first, rest)) => false
	end).

Definition empty := fun {T: Type} => @Nil (T)(tt).

Definition cons := fun {T: Type} => (fun x: T => (fun L: List T => Cons(x, L))).


Definition isCons := fun {T: Type} => (fun l: List T => match l with 
	| (Nil  u) => false
	| (Cons (first, rest)) => true
	end).

Definition singleton {T} (t: T): List T := cons t empty.

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
	| (Nil  u) => list
	| (Cons (first, rest)) => match ((eq x) first) with 
	| true => (((remove rest) x) eq)
	| false => ((cons first) (((remove rest) x) eq))
	end
	end.

Fixpoint generateList {T} (n: nat)  (f: (nat -> T)): List T := 
	match n with 
	| 0 => empty
	| S i => cons (f n) (generateList i f)
	end.

Fixpoint len {T} (list: List T): nat := 
	match list with 
	| (Nil  u) => 0
	| (Cons (first, rest)) => (1 + (len rest))
	end.

Fixpoint map {R T} (list: List T)  (func: (T -> R)): List R := 
	match list with 
	| (Nil  u) => empty
	| (Cons (first, rest)) => ((cons (func first)) ((map rest) func))
	end.

Fixpoint foldr {R T} (list: List T)  (base: R)  (func: (T -> (R -> R))): R := 
	match list with 
	| (Nil  u) => base
	| (Cons (first, rest)) => ((func first) (((foldr rest) base) func))
	end.

Definition sumAll (list: List nat): nat := 
	(((foldr list) 0) (fun x: nat => (fun y: nat => (x + y)))).

Definition forAll {T} (l: List T)  (op: (T -> unit)): unit := 
	(((foldr l) tt) (fun x: T => (fun u: unit => (op x)))).

Fixpoint addLast {T} (l: List T)  (x: T): List T := 
	match l with 
	| (Nil u) => (singleton x)
	| (Cons (first, rest)) => ((cons first) ((addLast rest) x))
	end.

Fixpoint isNotEmpty {T} (l: List T): bool := 
	match l with 
	| (Nil  u) => false
	| (Cons (_, rest)) => true
	end.

Definition smallerThanAll {T} (x: T)  (l: List T)  (comp: (T -> (T -> bool))): bool := 
	(((foldr l) true) (fun y: T => (fun r: bool => (andb r  ((comp x) y))))).

Fixpoint sorted {T} (l: List T)  (comp: (T -> (T -> bool))): bool := 
	match l with 
	| (Nil  u) => true
	| (Cons (first, rest)) => ((((smallerThanAll first) rest) comp) && ((sorted rest) comp))
	end.

Fixpoint insert {T} (l: List T)  (x: T)  (comp: (T -> (T -> bool))): List T := 
	match l with 
	| (Nil  u) => (singleton x)
	| (Cons (first, rest)) => match ((comp first) x) with 
	| true => ((cons first) (((insert rest) x) comp))
	| false => ((cons x) l)
	end
	end.

Fixpoint subset {T} (l1: List T)  (l2: List T)  (comp: (T -> (T -> bool))): bool := 
	match l1 with 
	| (Nil  u) => true
	| (Cons (first, rest)) => ((((contains l2) first) comp) && (((subset rest) l2) comp))
	end.

Definition permutation {T} (l1: List T)  (l2: List T)  (comp: (T -> (T -> bool))): bool := 
	((((subset l1) l2) comp) && (((subset l2) l1) comp)).

(*
{{True}}

FunctionDefinition(sort,List(T),Map(L -> List[Nat], comp -> (T -> (T -> Bool))),List[T],let l := L;
let sortedList := empty[T];
while (isNotEmpty[T](l)) {
	PatternMatch(l,List(((cons [first, rest]),let l := rest;
let sortedList := insert[T](sortedList)(first)(c))))};
sortedList)

{{((((sorted sortedList) c) = true)) /\ (((((permutation sortedList) L) c) = true))}}
*)

Theorem subset_refl: forall {T} (l: List T) c,
    subset l l c = true.
  Proof.
    Admitted.

Theorem append_empty: forall {T} (l: List T),
    append l empty = l.
  Proof.
    induction l. simpl. unfold empty. destruct u. auto.
    Admitted.


Theorem sorted_insert: forall {T} (l: List T) x c,
    sorted l c = true -> sorted (insert l x c) c = true.
  Admitted.

Theorem correct: forall {T} (l l' L rest sortedList: List T) (first: T)  c, ((((((((l = L)) /\ (sortedList = empty)) -> (((((sorted sortedList) c) = true)) /\ ((((((subset ((append sortedList) l)) L) c) = true)) /\ (((((subset L) ((append l) sortedList)) c) = true))))) /\ ((True) /\ (((((((((sorted sortedList) c) = true)) /\ ((((((subset ((append sortedList) l')) L) c) = true)) /\ (((((subset L) ((append l') sortedList)) c) = true)))) /\ ((isNotEmpty l' = true))) /\ (cons first rest = l')) /\ (l = rest)) -> (((((sorted (((insert sortedList) first) c)) c) = true)) /\ ((((((subset ((append (((insert sortedList) first) c)) l)) L) c) = true)) /\ (((((subset L) ((append l) (((insert sortedList) first) c))) c) = true))))))) /\ (((((((sorted sortedList) c) = true)) /\ ((((((subset ((append sortedList) l)) L) c) = true)) /\ (((((subset L) ((append l) sortedList)) c) = true)))) /\ (~(isNotEmpty l = true))) -> ((((((sorted sortedList) c) = true)) /\ ((((((subset ((append sortedList) l)) L) c) = true)) /\ (((((subset L) ((append l) sortedList)) c) = true)))) /\ (~(isNotEmpty l = true))))) /\ (((((((sorted sortedList) c) = true)) /\ ((((((subset ((append sortedList) l)) L) c) = true)) /\ (((((subset L) ((append l) sortedList)) c) = true)))) /\ (~(isNotEmpty l = true))) -> (((((sorted sortedList) c) = true)) /\ (((((permutation sortedList) L) c) = true)))))).
	Proof.
    myauto.
    - apply subset_refl.
    - rewrite append_empty. apply subset_refl.
    - apply sorted_insert. auto.
    - admit.
    - admit.
    - destruct l; [ | exfalso; apply H0; simpl; auto]. destruct u.
      rewrite append_Nil in H1.  simpl in H2.
      unfold permutation. apply andb_true_intro. split; auto.
      destruct p. auto.
Admitted
