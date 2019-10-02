## Synthesizing Network Control Planes

_The Problem_ Given a logical dataplane `L` and a concrete dataplane
`R`, we want to compute a transformation `f` from the logical control
plane rules `τ` to a set of concrete control plane rules, i.e. `f(τ)`
such that `R` with rules `f(τ)` is equivalent to `L` with rules `τ`.

We model networks using GCL with a nondeterministic choice operator
(`c [] c'`), an ordered choice operator (`c ▷ c'`), as well as full
boolean and arithmetic expressions. We also have a `t.apply()` which
indicates the application of a table `t` defined as in P4.

```
Var = Fields ∪ Holes
f ∈ Var
n ∈ BitVec
x ∈ TableName

# Commands
c ::=
  | skip   # Action
  | f := e # Action
  | c [] c'
  | c ▷ c'
  | c ; c  # Action
  | assert e
  | assume e
  | x.apply()

# BitVec Expressions
e ::= 
  | e ⊕ e , where ⊕ ∈ {+,-,*,/,&,|,∧,∨,=,<,>}
  | ¬ e
  | n
  | f
  
# Table Schemas
t ::= table x (κ*, a⁺, a₀(n₁,…, nₙ))

# Keys
κ ∈ Field List

# Actions
a ::= λx₁,…,xₙ. c


# Accessors
name(table x _) = x
keys(table _ (κ*, _, _)) = κ*
acts(table _ (_, a⁺, _)) = a⁺
defa(table _ (_, _, a₀(n₁,…, nₙ))) = a₀(n₁,…,nₙ)

# Table Accumulator
Tables(skip) = {}
Tables(f:=v) = {}
Tables(c ⊕ c') = Tables(c) ∪ Tables(c')
	⊕ ∈ {[], ▷, ;}
Tables(AS e) = {}
	AS ∈ {assert, assume}
Tables(table x _) = {x}	
```

We define `[]C` (sim `▷C`) where `C` is a set of commands to be the
finite nondeterministic (ordered) choice. I.e. if `C = {c₁,…, cₙ}`,
then `[]C =Δ= c₁ [] ⋯ [] cₙ` (sim` ▷C =Δ= c₁ ▷ ⋯ ▷ cₙ`).

We can also define `if e₁ → c₁ [] ⋯ [] eₙ → cₙ fi` as syntactic sugar
for `assert(⋁eᵢ);((assume e₁; c₁) [] ⋯ [] (assume eₙ; cₙ)`. Sometimes
we omit the `if` and `fi` for convenience. We define a similar
construct with `▷`.

Execution for most of the commands is fairly standard (excepting
tables). Note that assertions and assumptions are only well-typed when
their argument is a BitVec of length 1.

### Tables 

The exception is tables. A table schema is a triple of keys `κ*`,
actions `a⁺` and a default action `a₀` and the action data `n₁, ...,
nₙ` that are arguments for action `a₀`.  Notice that actions cannot be
full commands but are restricted to a simpler subset denoted above by
the `# Action` annotation. 

A table rule for a table `t` is a guarded command `⋀{κᵢ = mᵢ |
κᵢ ∈ keys(t)} → aⱼ(n₁,…,nₙ)`, for `aⱼ ∈ acts(t)`. A sequence of
matches `mᵢ`, action choices via `j` and action data `n₁,…,nₙ` for
action `j`, are provided by the control plane at runtime, producing
the corresponding table rules, which are combined via `▷`. The set of
triples of the form `((mᵢ | kᵢ ∈ keys(t)), j, (n₁,…,nₙ))` is written
`TableRule`. Each `(ms,j,ns) ∈ TableRule` has a corresponding command
(as above), we denote this command `comm (ms,j,ns)`. We can also write
`comm S =Δ= {comm (ms,j,ns) | (ms, j, ns) ∈ S}`.

We represent the controller's installation of rules via a mapping `τ :
TableName → (TableRule⁺)`. Since every table has a default action, the
controller has to install at least one rule, which is why we use `+`
instead of `*` in the codomain. We call `τ` a _table
instantiation_. Wwe say that `τ` is a _complete table instantiation
for a program P_ if `dom(τ) = Tables(P)`, and we'll write `τₚ` to
evoke this fact. We really only care about complete instantiations.

Then for a program `P`, we use postfix function application (`P τ`)
to represent the application of `τ` to the program `P` (reminiscient
of a substitution). We can define `P τ` inductively as follows:

```
(skip)      τ = skip
(f := e)    τ = f := e
(c [] c')   τ = c τ [] c' τ
(c ▷ c')    τ = c τ ▷ c' τ
(c ; c')    τ = c τ ; c' τ
(assert e)  τ = assert e
(assume e)  τ = assume e
(x.apply()) τ = ▷ comm τ(x)
```

We define our verification & synthesis problems in terms of this
function `τ`. But first, we need to know what it means to for one
program to implement another.

### Tableless Programs

It will be useful to define _tableless programs_, which are programs
that do not contain any table applications. These are programs that
require no controller interference, either because `τ` has already
been applied, or because there were no tables in the first
place. Formally,  we can define the predicate `tableless P` as
follows:

```
tableless(P) : Bool
tableless skip = true
tableless (assert e) = true
tableless (assume e) = true
tableless (c ⊕ c') for ⊕ ∈ {[], ▷, ;}
	= tableless c ∧ tableless c'
tableless (x.apply()) = false
```

### Implementation Predicate

Given two tableless programs `L` and `R`, we want to know whether `R`
is an implementation of `L`. That is, we want a predicate `implements`
that satisfies the following equation.

```
implements(L,R) ⇔ ∀ pkt. [| L |] pkt = [| R |] pkt
```

For now, we use symbolic weakest preconditions to do this analysis:

```
implements(L,R) =Δ= (wp(L) ↔ wp(R))

where 
wp(P) = wp(P, ⋀{v = v' | v ∈ fvs(P), v' fresh })
and
fvs(P) computes the free variables in P
```

Morally, we are computing the weakest preconditions on variables `v`
to produce the final values `v'`.


### Verification & Synthesis Tasks

Now we're ready to define our verification and synthesis tasks

#### Verification

We can define a the verification problem in a few different (but
equivalent) ways. In any case, its quite straightforward.

_Verification 1_ Given a tableless logical program `L` and a tableless
real/physical/concrete program `R`, we can verify whether `R` is an
implementation of `L` by asking whether the following formula is
satisfiable:

```
∀ v₁, v₂, …, vₙ. 
	implements(L,R)
where
{v₁,v₂, …, vₙ} = fvs(L) ∪ fvs(R)
```

_Verification 2_ Given a logical program `L`, a concrete program `R`,
and two complete instantions `τₗ` and `τᵣ`, we can ask

```
∀ v₁, v₂, …, vₙ. 
	implements(L τₗ, R τᵣ)
where
{v₁,v₂, …, vₙ} = fvs(L) ∪ fvs(R)
```


#### Synthesis

There are a few synthesis problems we can solve. The easier one, which
we have an effective prototype for, we call *online synthesis*. It is
the problem where we re-synthesize a logical table instantiation for
every control plane update. This problem can be phrased in two
different ways, which we will see below.

The more difficult synthesis problem is to synthesize a function that
takes in a logical table instantiation `τₗ` and produces a concrete
table instantiation `τᵣ` that will implement the logical program in
the real network.

We state the problems here:

_Online Synthesis 1_ Given a logical program `L`, a real program `R`,
and a complete table instantiation `τₗ`. We want to know that the
following is SAT. 

```
∃ τᵣ : (TableName → TableRule).
  ∀ x₁, …, xₙ.
    implements(L τₗ, R τᵣ)
```

The witness to the `∃τᵣ` is the collection of rules that we need to
install in the real network.


_Online Synthesis 2_ In this approach we synthesis a function. Given a
logical program `L` a real program `R` and a complete table
instantiation `τₗ`. We want to know that there is a witness to the
following formula.


```
∃ f : (TableName → TableRule⁺) → (TableName → TableRule⁺).
  ∀ x₁, …, xₙ.
    implements(L τₗ, R f(τᵣ))
```

Note that a witness `τᵣ` to _Online Synthesis 1_ can be made into a
witness to _Online Synthesis 2_ by setting `f = const τᵣ`.


_Offline Synthesis_ We synthesize a function as in *Online Synthesis
1*, but we swap the quantifier order. Given a logical program `L` and
a real program `R`, we synthesize a function `f` that witnesses the
following formula:

```
∃ f : (TableName → TableRule⁺) → (TableName → TableRule⁺).
  ∀ τₗ : TableName → TableRule⁺.
	∀ x₁, …, xₙ.
      implements(L τₗ, R f(τᵣ))
```

This will compute a fast function that can run effectively translate
the logical tables into real tables.
