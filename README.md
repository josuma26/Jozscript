# Jozscript

A programming language for formal verification.

Embedded Hoare logic to automatically generatye a Coq file with the necessary definitions and proof obligations to verify programs according to the provided specification.

Directory `proofs/` contains (mostly) autogenerated Coq files for verifying programs in `scala/programs`.

Implementaiton is based on PL literature I've read, but I tried to make the syntax as modern as possible.

Types:
```
t ::= Unit | Bool | Nat | t -> t | [t*t*...*t] (tuple) | {l1:t, l2:t, ... , ln:tn} (variant) | forall alpha, t (universal type) | alpha | t[t]
```

Can define custom types using the `type` keyword. To define lists, for exmample:

`type List := forall T, {nil: Unit, cons:[T, List[T]]}`

Not the `:=` for assignment instead of the more common `=`.

The List type is a variant with two constructors: `nil` of type `Unit` and `cons` which takes in a pair of type `T*List[T]`.

Expressions:
```
e ::= x | true | false | n | lambda x:t, e | e(e) | e[t] | match w with { | [pattern] => e ... | [pattern] => e }
      [e, e, ..., e] | e.i | l:e as t | e + e | e - e | e && e | e || e | (more Nat and Bool binary operators)
```
      
I tried to make pattern matching powerful enough to be used in a variety of use cases. Here are some tested examples:

Variants:

```
let x := some:3 as {some:Nat, none:Unit} 

match x with {
  | some:n => n + 3
  | none:unit => 0
}
```

Pattern-matching on value

```
match 5 with {
  | 0 => 0
  | 1 => 1
  | n => n-2
}
```

or more interestingly:

```
match [1, true, lambda x:Nat, x] with {
  | [num, bool, func] => func(num) || bool
}
```

Statements:
```
s ::= let x := e | while e do { s } | skip | type name := tau | def func[T, U, ...](l1: t, l2:t,..., ln:t): t := { e }
```

To make a recursive function:

```
def factorial(n: Nat): Nat := {
  match n with {
    | 0 => 1
    | n => n * factorial(n-1)
 }
}
```

Using generics (assuming the List type definition given above and some helpful constants):

```

let empty := Lambda T, nil:unit as List[T];
let cons := Lambda T, lambda x:T, lambda l: List[T], cons:[x, l] as List[T];

def map[T, R](list: List[T], func: T -> R): List[R] := {
  match list with {
    | nil:u => empty[R]
    | cons:[first, rest] => cons[R](func(first))(map[T][R](rest)(func))
  }
}
```
    
      
 
