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


Definition empty := fun {T: Type} => Nil(tt).

Definition cons := fun {T: Type} => (fun x: T => (fun L: List T => Cons((x, L)))).

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
	| i => Cons(((f i), ((generateList (i - 1)) f)))
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







Fixpoint update {U T} (m: Map T U)  (x: T)  (v: U)  (eq: (T -> (T -> bool))): Map T U := 
	(fun y: T => match ((eq x) y) with 
	| true => Some(v)
	| false => (m y)
	end).

(*
{{True}}

FunctionDefinition(findPath,List(T),Map(g -> Graph[T], from -> T, to -> T, eq -> (T -> (T -> Bool))),Option[List[T]],let stack := singleton[T](from);
let paths := lambda x: T, PatternMatch(eq(x)(from),List((true,VariantExpression(some,singleton[T](from),Option[List[T]])), (false,VariantExpression(none,tt,Option[List[T]]))));
let path := VariantExpression(none,tt,Option[List[T]]);
while (isCons[T](stack)) {
	PatternMatch(stack,List(((cons [first, rest]),let head := first;
let stack := rest)));
PatternMatch(paths(head),List(((some p),let pathTo := p)));
PatternMatch(eq(head)(to),List((true,let path := VariantExpression(some,pathTo,Option[List[T]]);
let stack := empty[T]), (false,let neighs := g.1(head);
while (isCons[T](neighs)) {
	PatternMatch(neighs,List(((cons [first, rest]),let neighs := rest;
PatternMatch(paths(first),List(((some p),tt), ((none u),let pathToNeigh := addLast[T](pathTo)(first);
let paths := update[T][List[T]](paths)(first)(pathToNeigh)(eq);
let stack := cons[T](first)(stack)))))))})))};
path)

{{(((pathFrom from) to) path)}}
*)

Theorem correct: forall (eq: (T -> (T -> bool))), forall (to: T), forall (from: T), forall (g: Graph T), forall (path: Option (List T)), forall (path': Option (List T)), forall (head: T), forall (head': T), forall (stack: List T), forall (stack': List T), forall (pathTo: List T), forall (pathTo': List T), forall (p: List T), forall (first: T), forall (rest: List T), forall (neighs: List T), forall (neighs': List T), forall (u: unit), forall (pathToNeigh: List T), forall (pathToNeigh': List T), forall (paths: (T -> Option List T)), forall (paths': (T -> Option List T)), (True) /\ ((True) /\ ((True) /\ ((((((((((((True) /\ ((True) /\ ((True) /\ ((True) /\ (True))))) /\ (stack = (singleton from))) /\ ((True) /\ (True))) /\ (paths = (fun x: T => match ((eq x) from) with 
	| true => Some((singleton from))
	| false => None(tt)
	end))) /\ ((True) /\ (True))) /\ (path = None(tt))) /\ ((True) /\ (True))) -> ((((andmap paths) (isPathFrom from))) /\ (((((pathFrom from) to) path)) /\ ((((((contains stack) x) eq) = true)) -> ((((pathFrom from) x) (paths x))))))) /\ ((True) /\ ((True) /\ (((True) /\ (((((((((((((((((andmap paths) (isPathFrom from))) /\ (((((pathFrom from) to) path')) /\ ((((((contains stack') x) eq) = true)) -> ((((pathFrom from) x) (paths x)))))) /\ ((isCons stack'))) /\ (((True) /\ ((True) /\ (True))) /\ (stack' = Cons((first, rest))))) /\ (head = first)) /\ ((True) /\ (True))) /\ (stack = rest)) /\ ((True) /\ (True))) /\ ((True) /\ ((paths head) = Some(p)))) /\ (pathTo = p)) /\ ((True) /\ (True))) /\ (true = ((eq head) to))) /\ (path = Some(pathTo))) /\ ((True) /\ (True))) -> ((((andmap paths) (isPathFrom from))) /\ (((((pathFrom from) to) path)) /\ ((((((contains empty) x) eq) = true)) -> ((((pathFrom from) x) (paths x)))))))) /\ ((True) /\ (((((((((((((((((((andmap paths) (isPathFrom from))) /\ (((((pathFrom from) to) path)) /\ ((((((contains stack') x) eq) = true)) -> ((((pathFrom from) x) (paths x)))))) /\ ((isCons stack'))) /\ (((True) /\ ((True) /\ (True))) /\ (stack' = Cons((first, rest))))) /\ (head = first)) /\ ((True) /\ (True))) /\ (stack = rest)) /\ ((True) /\ (True))) /\ ((True) /\ ((paths head) = Some(p)))) /\ (pathTo = p)) /\ ((True) /\ (True))) /\ (false = ((eq head) to))) /\ (neighs = (proj1 g head))) /\ ((True) /\ (True))) -> ((((andmap paths) (isPathFrom from))) /\ (((((pathFrom from) to) path)) /\ ((((paths head) = (some pathTo))) /\ ((((((contains stack) x) eq) = true)) -> ((((pathFrom from) x) (paths x)))))))) /\ ((True) /\ (((((((((((andmap paths) (isPathFrom from))) /\ (((((pathFrom from) to) path)) /\ ((((paths head) = (some pathTo))) /\ ((((((contains stack) x) eq) = true)) -> ((((pathFrom from) x) (paths x))))))) /\ ((isCons neighs'))) /\ (((True) /\ ((True) /\ (True))) /\ (neighs' = Cons((first, rest))))) /\ (neighs = rest)) /\ ((True) /\ (True))) /\ ((True) /\ ((paths first) = Some(p)))) -> ((((andmap paths) (isPathFrom from))) /\ (((((pathFrom from) to) path)) /\ ((((paths head) = (some pathTo))) /\ ((((((contains stack) x) eq) = true)) -> ((((pathFrom from) x) (paths x)))))))) /\ ((True) /\ ((True) /\ ((((((((((((((andmap paths') (isPathFrom from))) /\ (((((pathFrom from) to) path)) /\ ((((paths' head) = (some pathTo))) /\ ((((((contains stack) x) eq) = true)) -> ((((pathFrom from) x) (paths' x))))))) /\ ((isCons neighs'))) /\ (((True) /\ ((True) /\ (True))) /\ (neighs' = Cons((first, rest))))) /\ (neighs = rest)) /\ ((True) /\ (True))) /\ ((True) /\ ((paths' first) = None(u)))) /\ (pathToNeigh = ((addLast pathTo) first))) /\ ((True) /\ (True))) /\ (paths = ((((update paths') first) pathToNeigh) eq))) /\ ((True) /\ (True))) -> ((((andmap paths) (isPathFrom from))) /\ (((((pathFrom from) to) path)) /\ ((((paths head) = (some pathTo))) /\ ((((((contains ((cons first) stack)) x) eq) = true)) -> ((((pathFrom from) x) (paths x))))))))))))) /\ ((((((andmap paths) (isPathFrom from))) /\ (((((pathFrom from) to) path)) /\ ((((paths head) = (some pathTo))) /\ ((((((contains stack) x) eq) = true)) -> ((((pathFrom from) x) (paths x))))))) /\ (~(isCons neighs))) -> ((((andmap paths) (isPathFrom from))) /\ (((((pathFrom from) to) path)) /\ ((((((contains stack) x) eq) = true)) -> ((((pathFrom from) x) (paths x))))))))))))) /\ ((((((andmap paths) (isPathFrom from))) /\ (((((pathFrom from) to) path)) /\ ((((((contains stack) x) eq) = true)) -> ((((pathFrom from) x) (paths x)))))) /\ (~(isCons stack))) -> (((((andmap paths) (isPathFrom from))) /\ (((((pathFrom from) to) path)) /\ ((((((contains stack) x) eq) = true)) -> ((((pathFrom from) x) (paths x)))))) /\ (~(isCons stack))))) /\ ((((((andmap paths) (isPathFrom from))) /\ (((((pathFrom from) to) path)) /\ ((((((contains stack) x) eq) = true)) -> ((((pathFrom from) x) (paths x)))))) /\ (~(isCons stack))) -> ((((pathFrom from) to) path)))))).
	Proof.
Admitted.

Definition neighFunc := (fun x: nat => match x with 
	| 1 => ((cons 2) empty)
	| 2 => ((cons 3) empty)
	| 3 => ((cons 5) ((cons 4) empty))
	| 4 => ((cons 5) ((cons 1) empty))
	| 5 => empty
	end).

Definition list := ((generateList 5) (fun x: nat => (x + 1))).

Definition g := (list, neighFunc).

((((findPath g) 1) 5) (fun x: nat => (fun y: nat => (x = y))))