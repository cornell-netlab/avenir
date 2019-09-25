# Motley Basic Formulation

Given a logical program (with holes) `L` and a real program (with holes) `R`, we
want to synthesize a function `f` that maps from the holes in `L` the holes in
`R`. We want to encode the behavior of the programs into Z3. To do this we
compute the weakest preconditions of `L` and `R`. Then we construct various
verification conditions and pass them to Z3.


## The Language

The language we use to model network programs is a C-like language. It
is really a variant of Dijkstra's [guarded command
language](https://en.wikipedia.org/wiki/Guarded_Command_Language). Our
AST is the following

```
# Commands
c ::= 
  | skip          # No Operation
  | f := e        # Assignment
  | if m          # Selection
      b1 -> c1
	  ...
	  bn -> cn
    fi
  | assert b      # Assert Statements
  | assume b      # Assume Statements
  | c1;c2         # Sequencing

# if modifiers
m ::= total | partial | ordered

# Booleans
b ::= 
  | True | False
  | b1 && b2      # Conjunction
  | b1 || b2      # Disjunction
  | ~b            # Negation
  | e > e'        # Greater Than
  | e < e'        # Less Than 
  | e = e'        # Equality
  
# Expressions
e ::= 
  | n      # Integers
  | f      # Fields
  | ?      # Holes
  | e + e' # Addition
  | e - e' # Subtraction
  | e * e' # Multiplication
 

```

We also have `while b do c` loops, but they are unrolled during the
analysis. We can ignore them for now.

The only unusual constructs are the `if` `assert` and `assume` commands.

### Selection 

The selection statement (i.e. `if m (bi -> ci) fi` (ignoring the
modifier `m` for now) is a kind of nondeterministic if-statement. Executing a selection will test each `bi` (called a guard). For those `bi`s that evaluate to true, execution nondeterministically picks one of the `ci`s to run. Consider the following example program:
```
x := 5;
y := 7;
z := 0
if ... # Both branches are satisfied! pick one?
  ( x = 5 ) -> z := 5
  ( y = 7 ) -> z := 7
fi
```

Since both guards are true, i.e. `x = 5` and `y = 7` both hold, then
`z` will nondeterministically have the value `7`.

Now lets talk about the modifiers, `partial`, `total`, and
`ordered`. The `total` modifer says that if no guard holds, then the
program crashes. The `partial` modifier says that if no branch
succeeds, then the selection is treated like a `skip`. The `ordered`
modifier defined as the following syntactic sugar for all `i`:
```
if ordered
   ...
  (bi -> ci)
  ...
fi
==
if partial
 ...
 (bi && ~ b{i-1} && ~ b{i-2} .. && ~b{1} -> c_i
 ...
fi
```

In this definition we "and" the `i`th boolean guard with the negation
of the previous `i-1` boolean guards, thus preventing any ambiguity
and turning the selection statement into a more standard McCarthy-like
or C-like `if-then-else` statement.


### Assert

The `assert b` statement checks whether `b` evaluates to true. If it
does, the program aborts, if it is true, the program continues
executing.

Now we can see the following equation relating `if total` and `if partial`

```
if total
  ...
  (bi -> ci)
  ...
fi
===
assert (... && bi -> ci && ...)
if partial
  ...
  (bi -> ci)
  ...
fi
```
### Assume

The `assert b` command behaves like a `no-op` at runtime, its effect
is only visible in the predicate-transformer semantics. It effectively
behaves like a comment containing a logical precondition.


## Predicate Transformer Semantics – Weakest Preconditions

We can compute the weakest precondition with respect to a formula `φ`
in [the standard
way](https://en.wikipedia.org/wiki/Predicate_transformer_semantics)
for each of these syntactic constructs. The intuition behind the
weakest precondition of a program `ρ` is that it is the most-general
formula `ψ` such that packets satisfying `ψ` will satisfy `φ` after
they `ρ` is executed.

```
wp(skip, φ) = φ
wp(f := e, φ) = φ[f ↦ e]
wp(c₁; c₂, φ) = wp(c₁, wp(c₂, φ))
wp(assert b, φ) = b ∧ φ
wp(assume b, φ) = b ⇒ φ
wp(if total ... bᵢ -> cᵢ ... fi, φ) = (⋀bᵢ) ∧ (⋁(bᵢ ⇒ wp(cᵢ, φ)))
wp(if partial ... bᵢ -> cᵢ ... fi, φ) = (⋁(bᵢ ⇒ wp(cᵢ, φ)))
wp(if ordered ... bᵢ -> cᵢ ... fi, φ) = (⋁((bᵢ∧ ~b₁ ∧ ... ∧ ~bᵢ₋₁)⇒ wp(cᵢ, φ)))
```


### Symbolic Packet

To understand the behavior of a program ρ, we need to pass
through a proposition `φ` that caputures the behavior of the packet in
the network, we use a so-called "symbolic packet for program ρ", which is a formula
that equates each variable in the program to a symbolic variable,
e.g. for a program that contains `ipv4.dst`, `ethernet.smac`, and
`ethernet.dmac` we would create the following symbolic packet:

```
ipv4.dst = α 
∧ ethernet.smac = β
∧ ethernet.dmac = γ
```

So then, when we compute the weakest preconditions, the symbolic variables
represent the final values of the packet and the real variables represent the
initial values of the packet.  Often, for convenience, we will write `wp(ρ)` to
mean the weakest precondition of the program `ρ` with respect to the symbolic
packet of `ρ`.  For instance in the following (unrealistic) program:

```
if total
  ipv4.dst = 10 -> ethernet.dmac := 99
  ipv4.dst = 11 -> ethernet.dmac := 100
fi ;
ethernet.smac := 1
```

Now, we can compute the weakest precondition as follows:

```
wp(if total
    ipv4.dst = 10 -> ethernet.dmac := 99
    ipv4.dst = 11 -> ethernet.dmac := 100
  fi ; ethernet.smac := 1
  , ipv4.dst = α 
	∧ ethernet.smac = β
	∧ ethernet.dmac = γ)

=

wp(if total
    ipv4.dst = 10 -> ethernet.dmac := 99
    ipv4.dst = 11 -> ethernet.dmac := 100
  fi 
  , ipv4.dst = α 
	∧ 1 = β
	∧ ethernet.dmac = γ)

=


(ipv4.dst = 10 ∨ ipv4.dst = 11)
∧ ((ipv4.dst = 10 ⇒ wp(ethernet.dmac := 99
	                  , ipv4.dst = α 
				        ∧ ethernet.smac = β
						∧ 1 = γ))
   ∨ (ipv4.dst = 11 ⇒ wp(ethernet.dmac := 100
                        , ipv4.dst = α 
                          ∧ 1 = β
                          ∧ ethernet.dmac = γ)))
						  
=


(ipv4.dst = 10 ∨ ipv4.dst = 11)
∧ ((ipv4.dst = 10 ⇒ ipv4.dst = α 
				    ∧ 1 = β
					∧ 99 = γ)
   ∨ (ipv4.dst = 11 ⇒ ipv4.dst = α 
                      ∧ 1 = β
                      ∧ 100 = γ)))
					  
					  
== (by modus ponens)
(ipv4.dst = α ∧ 1 = β) 
∧ ((ipv4.dst = 10 ∧ 99 = γ)
   ∨ 
   (ipv4.dst = 11 ∧ 100 = γ))
					  

```

This final formula shows that the incoming packet doesn't modify the `ipv4.dest`
field (`ipv4.dest = alpha`, and the final `ethernet.smac` value is 1 (shown by
`1 = β`). Then it says that either the IPV4 destination is 10 (shown by
`ipv4.dst = 10`) and the final value of `ethernet.dmac` is 99 (shown by `99 =
γ`) or the IPV4 destination is 11 and the final value of `ethernet.dmac` is
`100` (shown by `100 = γ`). Notice that the inital values of `ethernet.dmac` and
`ethernet.smac` do not appear in the weakest precondition, which means that no
decisions in the program depend on those values.

## Verification

Assume we have a logical program `L` and a real program `R`. We want to verify
that `R` is an implementation of `L`. To do so we construct the following
formula and pass it to `z3` to solve:

```
∀ x₁, x₂, ..., xₙ.
   wp(L) ⇒ wp(R)
```

where each `x₁, x₂, ..., xₙ` is a free variables `L` or in `R`, such that `wp(L)
⇒ wp(L)` has no free variables.

This verifies that the input and output behavior of the two programs is the same.

## Simple Synthesis

Assume that we have a logical program `L` with no holes, and a real program `R`
with holes. We want to fill the holes in `R` such that `R` implements `L`,
i.e. we want to solve

```
∀ x₁, x₂, ..., xₙ.
  ∃ h₁, ..., hₘ. 
    wp(R) ⇒ wp(L[??ᵢ ↦ hᵢ])
```

i.e. there is a valuation on holes that maps each hole to a concrete value


## More General Synthesis

Assume we have a logical program `L` with holes, and a real program `R` with
holes. We want to find a mapping `f : Holes(L) → Holes(R)`, where `Holes(ρ)` is
the set of holes in the program `ρ`. So far we are passing the current formula
to Z3:

```
∃ f : Holes(L)ˡ → Holes(R)
  ∀ x₁, x₂, ..., xₙ.
    ∀ ?₁, ?₂, ..., ?ₗ.
        wp(R) ⇒ wp(L[??ᵢ ↦ f (?₁, ?₂, ..., ?ₙ)])
```

where `Holes(L) = {?₁, ?₂, ..., ?ₗ}` and `Holes(R) = {??₁, ??₂, ..., ??ₖ}`.


## Encoding in z3

Check out the [rise4fun tutorial](https://rise4fun.com/Z3/tutorial/guide) for an
introduction to z3.
