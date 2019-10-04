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

We can define an ordered choice operator as syntactic sugar:
`c ▷ c' =Δ= c ▷ (assert ¬wp(c,true); c')`

We define `[]C` (sim `▷C`) where `C` is a set of commands to be the
finite nondeterministic (ordered) choice. I.e. if `C = {c₁,…, cₙ}`,
then `[]C =Δ= c₁ [] ⋯ [] cₙ` (sim` ▷C =Δ= c₁ ▷ ( c₂ ▷ ( ⋯ ▷ cₙ) ⋯ )`).

We can also define `if e₁ → c₁ [] ⋯ [] eₙ → cₙ fi` as syntactic sugar
for `assert(⋁eᵢ);((assume e₁; c₁) [] ⋯ [] (assume eₙ; cₙ))`. Sometimes
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
`comm S =Δ= []{comm (ms,j,ns) | (ms, j, ns) ∈ S}`.

We also mimic P4's (statically enforcable) restriction that each `x ∈
TableName` can only occur once in each trace though the program. I.e.,
`x.apply() [] x.apply()` is okay, but `x.apply(); x.apply` is
disallowed.

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
    implements(L τₗ, R f(τₗ))
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
      implements(L τₗ, R f(τₗ))
```

This will compute a fast function that can run effectively translate
the logical tables into real tables.

#### Decomposing Offline Synthesis

The domain of `f` is huge, so we'd like to not have to accomodate all
of its possibilities. Nate's observation that "every packet only hits
one rule per table" can help us here.

We conjecture that we can synthesize a smaller function as
follows. Let `τₗ` pick out a single rule for each table in the logical
network, and `fₛ` constructs the rules required in the concrete
tables.

```
∃ fₛ : (TableName → TableRule) → (TableName → TableRule⁺).
  ∀ σₗ : TableName → TableRule.
	∀ x₁, …, xₙ.
      implements(L σₗ, R fₛ(σₗ))
```

From this smaller `fₛ`, we can construct our runtime `f` where `f(τₗ)`
is defined in the following way

```
f(τₗ)(x) = { fₛ(σₗ)(x) | σₗ ⊆ τₗ, σₗ complete}
```

We write `σ ⊆ τ` since converting `τ` to a relation by reversing the
powerset construction, i.e. `{(x, y) | x ∈ dom(τ), y ∈ τ(x)}` yields
`σ` as a subset of `τ`. Notice that `σ` does not range over all
subsets of `τ`, since it still must be a function.


We can say this construction is correct if

```
∀ L, R ∈ Commands.
  ∀ fₛ : (TableName → TableRule) → (TableName → TableRule⁺).
    (∀ σₗ : TableName → TableRule.
       ∀ x₁, …, xₙ. implements(L σₗ, R fₛ(σₗ)))
	⇒ (∀ τₗ : TableName → TableRule⁺.
		∀ x₁, …, xₙ. implements(L τₗ, R f(τₗ)))
```


_Proof Idea_. 
Let `L` and `R` be commands (possibly with tables). 
Let `fₛ` be a witness for  the offline synthesis problem s.t. the
following holds (called *assm1*).
```
∀ σₗ : TableName → TableRule. 
∀ x₁, …, xₙ. 
implements(L σₗ, R fₛ(σₗ))
```

Let `τₗ` be a complete table instantiation for `L`. 

We want to show that 
```
∀ x₁, …, xₙ. 
wp(L τₗ) ⇔ wp(R f(τₗ))
```

We need a slightly different semantics for programs:

```
tr(skip, τ, pkt) = (pkt, [])
tr(f:=e, τ, pkt) = (pkt[f↦eval(e, pkt)], [])
tr(c;c', τ, pkt) = (pkt'', trace' ++ trace '')
	where pkt', trace' = tr(c, τ, pkt)
	      pkt'', trace'' = tr(c', τ, pkt')

tr(assert e, τ, pkt) | eval(e, pkt) == #b1 = []
                     | eval(e, pkt) == #b0 = ⊥

tr(assume e, τ, pkt) = pkt

tr(t.apply(), τ, pkt) = trₜ((comm∘τ) t, pkt)

trₜ(rows, pkt) = ( [(t,((mᵢ)ₖ, j, (dᵢ)ₙ))]
	             , snd tr(actⱼ(d₁,…,dₙ),pkt))
	where
	∧ₖ(κᵢ = mᵢ) → actⱼ(d₁,…,dₙ) in rows 
	and eval(∧ₖ(κᵢ = mᵢ), pkt) == #b1 

```

Consider an input-outpu packet pair `pkt,pkt' =
(x₁,…,xⱼ)(x'₁,…,x'ₙ)`. Now, we can observe a trace of which tables are
executed and which table rule hit each action in the logical program,
namely, `σ,pkt = tr(L, τₗ, pkt)`. Notice that `σ` can also be thought
of as a partial single table instantiation for `L`. To extend it to a
full table instantiation, we can add additional mappings `t ↦
t.default` for every `t` not already in `σ`. We write this completed
`σ` as `σₗ`.

Consider the largest set `{σₗ₁,…, σₗₙ}` such that `⋃ᵢ₌₁ⁿσₗᵢ = τₗ`
and every `σₗᵢ` is complete. Now, for every `σₗᵢ` there is a formula
`φᵢ` such that `φᵢ = wp(L φₗᵢ)`. Show that `⋁ᵢⁿ φᵢ ⇔ wp(L τₗ)`.
```
wp(L τₗ)
⇔ wp (L ⋃ᵢ₌₁ⁿσₗᵢ) (defn σₗᵢs)
⇔ wp ([ᵢ₌₁]ⁿ L σₗᵢ) (by lemma1* (below))
⇔ ⋁ᵢ₌₁ⁿ (wp(L σₗᵢ)) (defn  wp)
⇔ ⋁ᵢⁿ φᵢ  (defn φᵢs)
```

We have one un-discharged goal from the above proof. The application
of lemma1 requires that `{σₗ₁,…, σₗₙ}` is closed. Notice first that
since every `σₗᵢ` is complete they all of the same domain, which is
`Tables(L)`.  Given `σₗᵢ` and `σₗⱼ`, and two disjoint subsets of
`Tables(τₗ)`, `D` and `D'` s.t. `D ∪ D = Tables(τₗ)`. Let `σₗ'` be defined as follows
```
σₗ'(x) = σₗᵢ(x) if x ∈ D
σₗ'(x) = σₗₖ(x) if x ∈ D'
```
First observe that `σₗ'` is complete for `L`. Then observe that it is
a subset of `τₗ`, so there is some `1 ≤ k ≤ n` such that `σₗ' =
σₗₖ`. Conclude that `{σₗ₁,…, σₗₙ}` is closed.

We've just shown that `⋁ᵢⁿ φᵢ ⇔ wp(L τₗ)`.

So, now we have a set `{σₗᵢ}ᵢ` of (completed) traces corresponding to
the `φᵢ`s. We can similarly decompose each `wp(R fₛ(σₗᵢ))` to get a
set of traces `{σᵣᵢⱼ}ⱼ` with corresponding formulae `ψᵢⱼ` such that
`wp(R fₛ(σₗᵢ)) ⇔ ⋁ⱼψᵢⱼ`. From *assm1*, we know `∀i.φᵢ ⇔ ⋁ⱼψᵢⱼ`, which
says that `⋁ᵢⁿ φᵢ ⇔ ⋁ᵢ⋁ⱼψᵢⱼ`. Call this *prop1*.

Now, all we need to show is `⋁ᵢ⋁ⱼ ψᵢⱼ ⇔ wp(R f(τₗ))`. The RHS is, by
definition, `wp(R {x ↦ fₛ(σₗ)(x) | complete σₗ ⊆ τₗ})`. Which is
equicalent to `wp(R ⋃ᵢ{x ↦ fₛ(σₗᵢ)(x)})`.

We need to show that `{fₛ(σₗᵢ)(x)}ᵢ₌₁ⁿ` is *closed*, so that by
*lemma1*, we can conclude `wp(R ⋃ᵢ₌₁ⁿ fₛ(σₗᵢ)(x)) ⇔ wp([]ᵢ₌₁ⁿ R
fₛ(σₗᵢ)(x))`.

By definition, `wp([]ᵢ R fₛ(σₗᵢ)) ⇔ ⋁ᵢwp(R fₛ(σₗᵢ)) ⇔ ⋁ᵢ⋁ⱼψᵢⱼ.` And
we're done by *prop1*.

*Q.E.D.*





We say that a set `T` of `P`-complete table instantiation functions is
*closed* iff for every `{τ₁,τ₂} ⊆ T` and for every partitioning `D, D'
⊆ Tables(P)` such that `D ∩ D' = ∅` and `D ∪ D' = Tables(P)`, there
exists a `τ₃ ∈ T` such that `τ₃(D) = τ₁(D)` and `τ₃(D') = τ₂(D')`.


*lemma1* 
For a program `P`, we need to show for every *closed* set of table
instantiation functions `{τ₁,…,τₙ}`, all of which are complete for
`R`, that `wp(P ⋃ₙτᵢ) ⇔ wp([]ₙ P τᵢ)`.


Proceed by induction on `R`.
```
[SKP] immediate since ∀ τ. skip τ = skip.
[ASN] immediate since ∀ τ, f, e. (f := e) τ = f := e 

[AST] immediate since ∀ τ, e. (assert e) τ = (assert e)
[ASM] immediate since ∀ τ, e. (assume e) τ = (assert e)

[CHC] Recall that (c [] c') τ = c τ [] c' τ. 
      We know wp(c  ⋃ₙτᵢ) ⇔ wp([]ₙ c  τᵢ) by IH(c)
      Further wp(c' ⋃ₙτᵢ) ⇔ wp([]ₙ c' τᵢ) by IH(c')
      So, wp((c [] c') ⋃ₙτᵢ)
		   ⇔ wp(c ⋃ₙτᵢ [] c' ⋃ₙτᵢ)
		   ⇔ wp([]ₙ c τᵢ [] []ᵢ c' τᵢ)
		   ⇔ wp([]ₙ (c τᵢ [] c' τᵢ)) by assoc&comm of []
		   ⇔ wp([]ₙ ((c [] c') τᵢ)) by def of subst.

[SEQ] Recall that (c;c') τ = (c τ; c' τ)
      We know  wp(c  ⋃ₙτᵢ) = wp([]ₙ c  τᵢ) by IH(c)
      Further, wp(c' ⋃ₙτᵢ) = wp([]ₙ c' τᵢ) by IH(c')
	  So, wp((c ; c') ⋃ₙτᵢ)
	      ⇔ wp(c ⋃ₙτᵢ ; c' ⋃ₙτᵢ)
		  ⇔ wp(([]ₙ c τᵢ) ; ([]ₙ c' τᵢ))
		  ⇔ wp([]ₙ (c τᵢ;[]ₙ c' τᵢ))
		  ⇔ wp([ᵢ]ⁿ (c τᵢ;[ⱼ]ⁿ c' τⱼ))
		  ⇔ wp([ᵢ]ⁿ[ⱼ]ⁿ(c τᵢ; c' τⱼ))
		  ⇔ ⋁ᵢⁿ⋁ⱼⁿ wp(c τᵢ; c' τⱼ))
		  ⇔ ⋁ᵢⁿ⋁ⱼⁿ wp(c τᵢ; c' τⱼ)
			  Since {τ₁,…,τₙ} is sequence closed, for every i, j, 
			  there exists a 1 ≤ k ≤ n such that τₖ(Tables(c)) =
			  τᵢ(Tables(c)) and τₖ(Tables(c')) = τⱼ(Tables(c')),so
		  ⇔ ⋁ₖⁿwp(c τₖ; c' τₖ)
		  ⇔ wp([ᵢ]ⁿ(c τᵢ; c' τᵢ)
		  
[TBL] Recall (x.apply()) τ = []{comm c | c ∈ τ(x)}
	 So, wp(x.apply() ⋃ₙτᵢ) 
		 = wp([ᵢ]ⁿ []{comm c | c ∈ τᵢ(x)})  by def, set theory
		 = wp([ᵢ]ⁿ x.apply() τᵢ)
```
