# Construction

## Functional Dependencies

We can compute Functional dependencies `→ ⊆ Field* × Action⁺` by
remembering, when we merge two tables `F × A` and `G × B` into `F×G ×
A×B`, that `F → A` and `G → B`. At this time, we can also compute
Hoare-functional dependencies for `{F} A {G}`.

## Hoare-Functional Dependencies

A Hoare-Functional dependency `→ ⊂ Field* × Action × Field⁺`, written
`F₁ × A → F₂` intuitively says that the value of the field `F₂` a
function of `F₁` and `A`.

For an action `A`, conclude `fvs(A) × A → bvs(A)` which is
reminiscient of a Hoare Triple. We also can write `F → F'` for
any field `f` not in `fvs(A)` and not in `bvs(A)`.


## Partitioning

`F`,`G` sets of field names, `A`,`B` lists of action-set
identifiers. For field `f` of type `τ`, we write `dom(f)` to be the
inhabitants of type `τ`. We can lift this up to `dom(F)` by taking the
cross product. For a set of actions `{act₁, act₂, …, actₙ}` with identifier
`a ∈ A`, we write `dom(a) = {act₁, act₂, …, actₙ}`. We lift this up to
`A` by taking the cross product of each `dom(a)`. We refer to both the
the field names `f` and the action set identifiers `a` with the
umbrella term _attribute_.

```
dom(F) = ∏ {dom(f) | f ∈ F}
  note that dom(F ∪ F') ≅ dom (F' ∪ F)
  
dom(a::A) = dom(a) × dom(A)
```

A table `T` is a pair of sets of attributes that can written as the
pair `(F, A)`, along with the functional dependency `F → A`. We write
`keys(T) = F` and `acts(T) = A`.

We say that a relation `t` is in the domain of a table `T = (F,A)` iff
it is a function of type `F ⇀ A`. We say it is total if `t : F →
A`. We denote this as `t ∈ dom(T)`. Define `[|t|] : Pkt → Pkt` as
```
[|t|] pkt = [|t(π(F,pkt))|] pkt
```

We conjecture that given two sequenced tables `(F, A) = T` and `(G, B)
= S`, such that `T;S` is in SSA with no free variables, there exists
an `R` and a corresponding set of functional dependencies `RD` such
that for every total relation `r ∈ dom(R)` satisfying every `X → Y ∈
RD`, there exist total relations `t ∈ dom(T)` and `s ∈ dom(S)` such
that `∀ pkt [|r|] pkt = [|s|] ([|t|] pkt)`.

We can produce such relations and sets of functional dependencies as follows:
```
compose : Table → Table → (Table × FD)
compose T@(F,A) S@(G,B) =
 ((F∪Δ(A,G), A ++ B)
 , {F → A, G → B} ∪ FD(T) ∪ FD(S) ∪ HFD(A,G))
```


The following function computes the dominating set for a single field according to an action
```
δ : Action × Var → |P(Var)
δ(skip, f) = {f}
δ(assert b, f) = {f} ∪ fvs(b)
δ(assume b, f) = {f} ∪ fvs(b)
δ(g := e, f) | f  = g = fvs(e)
             | f <> g = {f}
δ(c;c', f) = 
  let V' = δ(c', f) in
  ⋃ { δ(c, v) | v ∈ V'}
  
δ(c [] c', f) = δ(c,f) ∪ δ(c', f)
```

We can lift this to sets of fields and sequences of actions

```
Δ : Action × |P(Var) → |P(Var)
Δ(a, V) =  ⋃ {δ(a, v) | v ∈ V}
```


_Proposition_. Given an action `a`, and a field `f`, and `{v₁,…,vₙ} =
δ(a, f)`, for every `x₁,…, xₙ ∈ dom(v₁), …, dom(vₙ)` then `x([| a |]
{v₁ ↦ x₁,…, v₁ ↦ xₙ}).f` is well defined.

_Proof_ TODO. By structural induction on `a`. _QED_

The corresponding proposition for `Δ` falls out as an immediate
consequence.

_lemma_  ∀ es. [|b|] pkt[fvs(b)↦ es] = [|b|] ·[fvs(b)↦ es]


_lemma_ `∀ a, f, v, pkt. [|a|] pkt[δ(a, f) ↦ v].f = ([|a|](·[δ(a, f) ↦
  v])).f`.

Proceed by induction on `a`.
+ (a = skip)
  δ(skip, f) = {f} 
  [|skip|] pkt[f ↦ v].f 
  = pkt[f ↦ v].f = v 
  and 
  ([|skip|] (·[f ↦  v])).f 
  = (·[f ↦  v])) = v
  
+ (a = assume b (wlog assert))
  δ(assume b, f) = {f} ∪ fvs (b)
  by previous lemma
  [|b|] pkt[fvs(b) ↦ vs] = [|b|] ·[fvs(b)↦ vs]
  so lhs is {} iff rhs is {}
  assume neither is {}, then
  [|assume b|] pkt[f ↦ v].f 
  = pkt[f ↦ v].f = v 
  and 
  ([|assume|] (·[f ↦  v])).f 
  = (·[f ↦  v])) = v
  
+ (a = g := e) for g <> f. sim to skip
  
+ (a = f := e) 
  δ(f:=e, f) = fvs (e)
  by previous lemma
  [|e|] pkt[fvs(e) ↦ vs] = [|e|] ·[fvs(e)↦ vs]
  Lets call this value v.
  Then ([|f:=e|] pkt[fvs(b) ↦ vs]).f
  = (pkt[fvs(b) ↦ vs] [f ↦ v]).f
  = v
  = (·[fvs(b) ↦ vs] [f ↦ v]).f
  = ([|f:=e|] ·[fvs(b) ↦ vs]).f
  
+ (a = a;a') By Chaining IHS TODO

+ (a = a [] a') Wlog by IH(a) TODO
  
_QED_

Then we can define the Hoare-Functional dependency as
```
HFD(A,F) = Δ(A,F)∪A → F
```

Let `(R,D) = compose(T,S)`. We can produce a function `impl :
dom(R) → dom(S) × dom(T)`

```
impl :: row(R) -> row(T) × row(S)
impl ρ = 
  let fs = π(keys(T), ρ) in
  let as = π(acts(T), ρ) in
  let a = reduce (;) as in
  let pkt = ·[Δ(a, keys(S)) ↦ fs] in
  let pkt' = [|a|] pkt in
  let gs = pkt'.keys(S) in
  let bs = π(acts(S), ρ) in
  (fs ++ as, gs ++ bs)
  
implDom :: dom(R) → dom(T) × dom(S)
implDom r = {impl ρ | ρ ∈ r }
```





_lemma_. For `ρ ∈ row(R)`, and `τ, σ = impl ρ`, `∀ pkt. [|ρ|] pkt =
[|τ|]([|σ|] pkt)`.

_proof_. Fix `ρ`, and `pkt` to show `[|ρ|]pkt = [|σ|]([|τ|]
pkt)`. There are two cases (1) `pkt` matches `ρ` and (2) otherwise:

1. Assume `keys(ρ) = pkt.keys(R)` Then we know that `keys(τ) =
   pkt.keys(T)` and `π(acts(T), ρ)) = acts(τ)`, and also that
   `π(acts(S), ρ) = acts(σ)`. To conclude we must show that `keys(σ) =
   ([|τ|] pkt).keys(S)`. First, observe that because of our
   assumption, `[|τ|] pkt = [|a|] pkt`.
   
   By construction of `impl`, we know that `keys(σ) = [|a|] ·[Δ(a,
  keys(S)) ↦ fs].keys(S)`. By lemma, `∀ a, S, pkt. [|a|] pkt[Δ(a,
  keys(S)) ↦ fs].keys(S) = [|a|] ·[Δ(a, keys(S)) ↦ fs].keys(S)`, so
  we're done.
  
2. Assume `keys(ρ) ≠ pkt.keys(R)`, then `[|ρ|]pkt = miss `. Assume further that
   `[|τ|] pkt ≠ miss` since otherwise we'd be done. This gives us that ̀τ(pkt) =
   ρ.acts(T)`, let `a = reduce (;) ρ.acts(T)`. So it must be that there's some
   `f ∈ keys(S)` such that `ρ.f ≠ pkt.f.`
   
   Let `pkt' = [|a|] pkt`. Show that `[|σ|] pkt' = miss`.  i.e. that that
   `pkt'.keys(S) ≠ keys(σ)`.
   
   Consider the field `f`. If `{f} = δ(a, f)`, then we're done. So
   assume —- BORkeD


_Finishing the Proof_ Let `(R, D) = compose(T,S)`. Let `r ∈ dom(R)`
respecting `D` and `(t,s) = implDom r` and show that `[|r|] = [|s|] ∘
[|t|]`.

We can extend these relations with their input and output packets, so
we really want to show that 
```
π(pkt₀₁, pkt₀rpkt₁) 
= π(pkt₀₁, pkt₀tpkt₂ ⋈ pkt₂spkt₁)
```

Assuming we've minimized our action sets up to full program
equivalence, this is equivalent to 
```
π(pkt₀,acts(S), pkt₀ ⋈ r) 
= π(pkt₀,acts(S), pkt₀ ⋈ t ⋈_{[|acts(t)|] pkt₀ = pkt₂} pkt₂ ⋈ s) 
```

We know that `Δ(acts(t), keys(s)) acts(t) → keys(s)`, and that
`Δ(acts(t), keys(s)) ⊆ keys(t).

_example_
```
A = {x:=1}, {y:=2}
B = {a₁, a₂}
SD -> A (T1)
SY' -> B (T2)

Y {x:=1; y':=y} -> Y' (T1A1)
{y':=2} -> Y'         (T1A2)
Y A -> Y'             (T1AY)
X A -> X'             (T1AX)
(SD, A); (SY', B)

{x:=1}⊕{y:=2}
a₁⊕a₂
(S,D,Y, {x:=1}, {y':=2}, a₁, a₂)  ——>        (SD, A); (SY', B)
(1,2,3, 0,1, 1,0)   execute action         (1,2, 0,1); (1,2, 1,0)
(1,2,_, 0,1, 1,0)  (must be a₁ by T1A)     (1,2, 0,1); ''
                   (must be y':=2 by T1)
(1,3,2, 1,0, 0,1)   xxxxxxxxxxxxxxx        (1,3, 2,0); (1,2, 0,1) 
(_,_,_, 1,0, 0,1)                          (_,_, 1,0); (_,_, 0,1)
```

The problem is that we have two `Y`s mapping to the same `Y'`.

Let `ρ₀, ρ₁` be two rows in a table of type `(S,D,Y, {x:=1; y:=y'}, {y':=2}, a₁,
a₂)`, Let `actᵢ = π(A,ρᵢ)` and for some `s₀,s₁ ∈ S`, let `pktᵢ = {s↦vᵢ, y ↦
ρᵢ.y]`.  We want to enforce the condition that if `[|act₀|](pkt₀).sy' =
[|act₁|](pkt₁).sy'`, then `π(B,ρ₀)= π(B,ρ₁)`. Which is to say:

`[|A|](SY).SY' → B`
`π(SY',SY ⋈ [|A|]) → B`

Intuitively, this condition maintians the functional constraint `SY → B`.

in general, for `(F,A);(G,B)`, we need the constraint 
`π(G, Δ(A,G) ⋈ [|A|]) → B`

For a set of actions `A`, define `[|A|] pkt ≜ {[|a|] pkt | a ∈
A}`. Relationally, this is `{(pkt, pkt') | [|a|] pkt, a ∈ A, pkt ∈ Pkt }`.







# Conjecture

Given two tables `F × A = T` and `G × B = S` with functional dependencies
`FD(T)` and `FD(S)`, and Hoare-Functional dependencies `HFD(T)` and `HFD(S)`, we
can implement the logical `T` via the physical `S` if(f?) the following
properties hold
1. `∃ α : A → B₀, B₀ ⊆ B, ∀ a ∈ A. [|a|] = [|α(a)|]`, 
2. there exists a "smallest" `α`, denoted `αₘ` where `α : A → B₁ < α A
  → B₂` iff `|B₁| < |B₂|`.
3. `F ⊆ G`, and
4. `⋀FD(T)[αₘ] ∧ ⋀HFD(T) ∧ ⋀HFD(S) ⇒ ⋀ FD(S)`

## Proof

Let `F × A = T ` and `G × B = S` be tables with functional
dependencies `FD(T)` and `FD(S)`. 




# Examples

## Destination One - Two Overwrite

```
S = {Src}
D = {Dst}
A = { {op:=0}, {op:=1} }
c = apply(sd, S x D, A)
d = apply(s, S, A₀); apply(d, D, A₁)
```

We need to convert `d` into a single table, specifically, `d =
apply(sd', S × D, A₀ × A₁)`, where `Aᵢ = A` with the additional
functional dependencies `S → A₀` And `D → A₁`.

### Logical = c, Physical = d

Notice that the actions are the same, so we can map logical actions
`A` to `A₁` via the identity – it doesn't matter what action we select
for `A₀` since it will be immediately overwritten.

Now we must map the keys `S × D`, with `S × D → A` to `S × D` with `S
→ A` and `D → A`. This is not possible, because `S × D → A` does not
imply `D → A`

### Logical = d, Physical = c

Again, we map actions `A₁ → A` and just throw away the action `A₀`
(since it is subsumed by `A₁`. We must map the keys `S × D` with 
`D → A` to `S × D` with `S × D → A`. This is possible, because `D → A`
proves `S × D → A` by Weakening.

``` Proof of Weakening Assume X → Z, to show `XY → Z`
1. X → Z (assumption)
2. XY → ZY (augmentation)
3. XY → Z (decomposition)
```


## Source-Destination One-Two Interact

```
S = {src}
D = {dst}
dom(A₀) = {{op:=1}, {op:=0}}
dom(A₁) = {{op:=1}, {op:=0}}
dom(A₂) = {{op:=1}, {skip}}
c = apply(P, S × D, A₀)
d = apply(Q, S, A₁); apply(d, D, A₂)
```

We need to convert `d` into a single table, which we do as 

```
apply(sd', S × D, A₁ × A₂), with
S → A₁
D → A₂
```

Now we have a more complicated mapping of actions

```
op:=1 can map to (op:=1, op:=1), (op:=1, skip), (op:=0, op:=1)
op:=0 can map to (op:=0, skip)
```

### Logical = d, Physical = c (Possible)

The logical table has the constraints that `S → A₁` and `D →
A₂`. Since `A₁` and `A₂` are both mapped to `A₀`, we need to show that
`S → A₀` and `D → A₀` can prove `S × D → A₀`.

```
1a. S → A₀ (assumption)
1b. D → A₀ (assumption)
2. S × D → A₀ × A₀ (Composition)
3. S × D → A₀ (Decomposition)
```

### Logical = c,  Physical = d (Impossible)

Our logical table has the determination that `S × D → A₀`, and our
physical table has restrictions `S → A₁` and `D → A₂`. Since `A₀` will
always map to both `A₁` and `A₂`, we need to show that `S × D → A₀` is
sufficient to prove `S → A₀` and `D → A₀`.

However, since `S × D` is a candidate key, it is not possible to make it any
smaller, and we conclude that this is impossible.



## Interleave 

```
D = {Dst}
A = { {x:=1; y:= 9}, {x:=2; y := 10} }
X = { {x:=1}, {x:=2} }
Y = { {y:=9}, {y:=10} }
c = apply(d, D, A)
d = apply(dx, D, X); apply(dy, D, Y)
```

We need to combine `d` into a single table
```
apply(dxy, D, X × Y) 
with D → X
 and D → Y
```

### `c` → `d` (possible)

Notice that `A` maps to (a subset of) `X \times Y`, so we need to show
that `D → X × Y` proves `D → X` and `D → Y`, which it does by
decomposition.

### `d` to `c` (impossible)

Theres no map from `X × Y` to `A`, because there is no equivalent to
the pair `(x:=1, y:=10)` in `A`.

However if there were, our FDs would not be violated, because `D → X`
and `D → Y` proves `D → X × Y` by Composition and CrossIso.

```
Proof of CrossIso
If XX → Y, then X → Y
1. XX → Y (assumption)
2. X → X (Identity)
3. X → XX (Union 2, 2)
4. X → Y (Transitivity 1, 3)
```

## Dependency

```
X = {x}
Y = {y}
SetY = { {y := x}, {y := 1} }
SetZ = { {z := 100}, {skip}}
SetYZ = { {y:= x}, {y:= 1}, {y := x; z := 100}, {y:= 1; z := 100}}
c = apply(xsety, X × Y, SetY); apply(gety, Y, SetZ)
d = apply(xy, X × Y, SetYZ)
```

First we need to combine `xsety` and `gety` into a single table, which
we do:

```
apply(xy', X × Y × Y', SetY × SetZ)
with X × Y → SetY
 and Y' → SetZ
 and SetY → Y'
 ```

Notice that `SetY × SetZ` and `SetYZ` are isomorphic by the function
`map (uncurry (;))`, so we'll use `SetY × SetX` WLOG.

### `c` → `d` (impossible)

Given `FD(xy')`, namely `X × Y × Y' → SetY × SetZ`, `X × Y → SetY`, `Y'
→ SetZ` and the observation `SetY → Y'`, we need to show all fds in
`FD(xy)` hold , namely `X × Y → SetYZ`, or equivalently `X × Y → SetY
× SetZ`.

```
0. X × Y × Y' → SetY × SetZ (Assumption on xy')
1. X × Y → SetY (Assumption on xy')
2. Y' → SetZ    (Assumption on xy')
3. SetY → Y'    (Observation on xy')
4. X × Y → Y'  (Transitivity 1,3)
5. X × Y → SetZ (transitivity 1,4)
6. X × Y → SetY × SetZ (union 1,5)
```

### `d` → `c` (impossible)

Given `FD(xy)`, namely `X × Y → SetYZ`, or equivalently `X × Y → SetY
× SetZ` and the observation `SetY → Y'`, we need to show all fds in
`FD(xy')` hold, namely (a) `X × Y × Y' → SetY × SetZ`, (b) `X × Y →
SetY`, (c) `Y' → SetZ`

```
0. X × Y → SetYZ (Assumption on xy)
1. SetY → Y'     (Obsevation on xy')
2. X × Y → SetY × SetZ (Equivalence of SetYZ and SetY × SetZ)
3. X × Y × Y' → SetY × SetZ (Weakening 2) (goal a)
4. X × Y → SetY (Decomposition 2) (goal b) 
```

Unfortunately, there's no way to prove (c) `Y' → SetZ` because we dont
know how to map the set `{{y := 1}, {y:= 1; z:=100}} ⊆ SetYZ`, since
for the same value of `y'`, we execute two different members of
`SetZ`, `{skip}` and `{z := 100}`.



## Partial Dependency

```
X = {x}
Y = {y}
SetY = { {y := 47}, {skip} }
SetZ = { {z := 100}, {skip}}
SetYZ = { {skip}, {y:= 47}, {z:= 100}, {y := 47; z := 100} }
c = apply(xsety, X × Y, SetY); apply(gety, Y, SetZ)
d = apply(xy, X × Y, SetYZ)
```


We need to turn `c` into a single table `xy'`:

```
c = apply(xsety, X × Y × Y', SetY × SetZ)
with X × Y → SetY
 and Y' → SetZ
 and Y × SetY → Y'
```


### `c` → `d`

We need to show that from `X × Y → SetY` and `Y × SetY → Y'` and `Y' →
SetZ` we can prove `X × Y → SetY × SetZ`.

```
0. X × Y → SetY  (assumption)
1. Y' → SetZ     (assumption)
2. Y × SetY → Y' (observation)
3. X × Y → Y     (Reflexivity)
4. X × Y → Y × SetY (Union 3, 0)
5. X × Y → Y'    (Transitivity 4, 2)
6. X × Y → SetZ  (Transitivity 5, 1)
7. X × Y → SetY × SetZ (Union 0, 6 )
```


### `d` → `c`
Impossible for the same reason as (Dependency)
