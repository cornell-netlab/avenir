Command
c ::=
    | Skip
    | f ← e
    | c;c
    | t.apply()
    | if b → c [] ⋯ [] b → c fi

Expressions e
Booleans b
Fields f

Action
a ::= λ x₁,…,xn → c

Tables
t ∈ TableName × Field List × Action List

Entry
ρ ∈ Expression List × BExpr × Action Data × Action ID

(ipv4, [ipv4.dst], [λ x. out ← x, drop, skip])

( [124.0.0.1], true, [1], 0)
assume ipv4.dst = 124.0.0.1; out ← 1

Instantiation Function
τ :: TableName → Entry list


EDITS
e ::= Add(table, ρ)
    | Del(table, i)

EDIT HOLES
e ::= ?Addᵢ(t)
    | ?Del(t,i)


Incorporating edits and edit holes
τ + Add(t, matches, data, a) = τ{t ↦ τ(t) ++ (matches, ⊤, data, a)}
τ + Del(t, i) = τ{t ↦ τ(t)[0:i] ++ τ(t)[i+1:]}


τ + ?Addᵢ(t) = τ{t ↦ τ(t) ++ [(?matches, ?AddᵢTo_t = 1, ?data, ?a]}
τ + ?Del(t,i) = add ?Del_t_i = 0 to τ(t)[i]


let cegis (l : cmd)                     (p : cmd)
          (τ : TableName -> entry list) (σ : TableName → entry list)
	  (es : edit list)              (rs : edit list)
	  (χs : Packet² list) 
	  =
  match VALID(l (τ + es) = p (σ + rs)) with
  | valid -> Success rs
  | CEX χ -> solve l p τ σ es rs (χ:χs) ⊤

and solve l p τ σ es rs (χs : Packet² list) (φ : formula) =
  if SAT(φ) == unsat then Fail else
  match get_model' p σ rs χs φ with
  | unsat -> Fail  
  | sat rs' ->
     assert rs' ⊧ φ;
     match cegis l p τ σ es (rs + rs') χs with
     | Success rs'' -> Success rs''
     | Fail ->
       assert |φ| > |φ ∧ ¬rs'|;
       solve l p τ σ es rs χ (φ ∧ ¬rs')

+——————————————————————————————————–+
Thm (Completeness).
  ∀ l  logical program,
    p  physical program,
    τ  logical instantiation,
    σ  physical instantiation,
    es logical edits,
    rs physical edits,
    χs counterexamples seen so far
    φ  formula restricting the search space of models.

    [cegis]
    1.  if ∃ rs'
      	   st. l (τ + es) = p (σ + rs')
	and ∀ (in, out) ∈ χs.
	      〚l (τ + es)〛in = out = 〚p (τ + rs)〛in
        then ∃ rs'' 
          st. Success rs'' = cegis l p τ σ es rs χs
              ∧ l (τ + es) = p (σ +  rs'')

    [solve]
    2. if ∃ rs' ⊧ φ
          st. l (τ + es) = p (σ + rs')
       then ∃ rs'' ⊧ φ
         st. Success rs'' = solve l p τ σ es rs χs φ
           ∧ l (τ + es) = p (σ + rs'')
+——————————————————————————————————–+
Proof by mutual induction on (|Packet² - χs|, |{m ⊧ φ}|).
– Base Case (0,0).
  Prove condition 1.
    intros *. 
    let rs' be st l (τ + es) = p (σ + rs') (assn1)
    assume ∀ (in, out) ∈ χs. 〚l (τ + es)〛in = out = 〚p (τ + rs)〛in (assn2)
    since |Packet² - χs| = 0, χs = Packet².
    Then, by assn2
    ∀ (in, out) ∈ Packet². 〚l (τ + es)〛in = out = 〚p (τ + rs)〛in
    which is the definition of
    l (τ + es) = p (τ + rs) ✓

  Prove condition 2.
    intros *.
    Since |{rs ⊧ φ}| = 0,
    there does not exist rs' ⊧ φ,
    and so we're done. ✓
       
- Inductive case (n+1, m+1)
  Prove Condition 1 [cegis].
    intros *.
    let rs' be st l (τ + es) = p (σ + rs') (assn1)
    assume ∀ (in, out) ∈ χs.
      〚l (τ + es)〛in = out = 〚p (σ + rs)〛(assnχ)
    There are two cases:
    Case 1.
    In this case, l (τ + es) = p (σ + rs) is valid, so
    rs'' = rs and we're done.
    Case 2.
    In this case, χ is a witness to the invalidity of l (τ + es) = p (σ + rs).

    We need to show that
      ∃ rs'' ⊧ ⊤ st. Success rs'' = solve l p τ σ es rs (χ:χs) ⊤
                 ∧ l (τ + es) = p (σ + rs'')
    since cegis returns solve l p τ σ es rs (χ:χs) ⊤ in this case.

    By (assnχ), χ is not a member of χs, so
        |Packet² - χs| > |Packet² - (χ:χs)|
    and correspondingly,
       (|Packet² - χs|, |{m⊧⊤}|) > (|Packet² - (χ:χs)|, |{m⊧⊤}|}
    which means that by condition 2 of the IH,
       if ∃ rs' ⊧ ⊤ st l (τ + es) = p (σ + rs')
       then ∃ rs'' ⊧ ⊤
         st. Success rs'' = solve l p τ σ es rs (χ:χs) ⊤
           ∧ l (τ + es) = p (σ + rs'')

    All that remains is to show
       if ∃ rs' ⊧ ⊤ st l (τ + es) = p (σ + rs')

    Since every sequence of edits models ⊤, show
       if ∃ rs' st l (τ + es) = p (σ + rs')

    which follows by (assn1).
     
    

  Prove Condition 2. [solve]
    Let rs' ⊧ φ and l (τ + es) = p (σ + rs')
    to show ∃ rs''' ⊧ φ
         st. Success rs''' = solve l p τ σ es rs χs φ
           ∧ l (τ + es) = p (σ + rs'')

    We know φ is sat because rs ⊧ φ, so we are in the else case of solve.
    
    TODO :: Prove that for every rs' there exists rs⁰ st.  rs is
    σ-normalized and σ + rs' is tight.

    Then rs⁰ is a witness for the get_model condition – equivalence on
    all inputes is stronger than equivalence on only the counterexamples.

    So by that lemma, rs'' = get_model' p σ rs χs φ
     ∧ ∀ (in,out) ∈ χs,
         [|p (σ + rs + rs'')|] in = out

    Then we bubble up to the cegis loop which terminates under the
    proves the goal

□

*===================================*


let get_model p σ rs χ@(pkt₀,pkt₁) φ =
  let σ' = σ + rs + holes(p,1,σ + rs) in
  SAT(φ ∧ (pkt₀ ⇒ wp(p σ, pkt₁)))

let get_model' p σ χs φ =
  let σ' = σ + holes(p,|χs|, σ) in
  SAT(φ ∧ ⋀{in ⇒ wp(p σ', out) | (in,out) ∈ χs})

+———————————————————————————————–+
Lemma.
∀ p  program,
  σ  instantiation,
  χs counterexamples.
if ∃ rs st
     ∀ (in,out) ∈ χs,
      〚p (σ + rs)〛 in = out
then ∃ rs' st.
       rs' = get_model' p σ χs φ
       ∧ ∀ (in,out) ∈ χs,
         〚p (σ + rs')〛in = out
+————————————————————————————————+
intros *.

Let rs st ∀ (in,out) ∈ χs, 〚p (σ + rs)〛in = out (1)

Restrict σ + rs to only the entries hit by some counter example in χs.
Call this new instantiation σ'.

Notice that every table in σ' has |χs| rows or fewer, and that
p (σ + rs) =χs= p σ' (2)

Consider the following model for holes(p, |χs|, σ):
  ds = for t ∈ tables(p), and 0 ≤ i < |σ(t)|,  Del(t,i)
  as = tables(p)
     >>| λ t → [ Add(t, σ'(t)[i]) | 0 ≤ i < |σ'(t)| ]

  rs' = ds + as

Notice that σ + rs' = σ + ds + as = · + as = σ'

Conclude (by 1,2) that ∀ (in,out) ∈ χs, 〚p (σ + rs')〛in = out
*================================*

#### for t in tables(p), for i ∈ [0,n)  ?Addᵢ(t) ∈ holes(p,n,τ)
#### for t ∈ tables(p), and 0 ≤ i < |τ(t)|,  ?Del(t,i) ∈ holes(p,n,τ)
let holes (p, n, τ) =
    tables(p) >>= (λ t → repeat n ?Add(t))
    ++
    tables(p) >>= (λ t → [?Del(t,i) | 0 ≤ i < |τ(t)| ])



No Backtracking!

let cegis l p τ σ es χs =
  match l τ = p (σ + es) with
  | Valid → Success es
  | Cex χ →
     match get_model' p σ es ({χ}∪χs) with
     | unsat → fail
     | sat es' → cegis l p τ σ es' ({χ}∪χs)
     
+——————————————————————————————————–+
Thm (Completeness).
  ∀ l  logical program,
    p  physical program,
    τ  logical instantiation,
    σ  physical instantiation,
    rs physical edits,
    χs counterexamples seen so far

    if ∃ rs'
       st. l τ = p (σ + rs')
    and ∀ (in, out) ∈ χs.
      〚l τ〛in = out = 〚p (σ + rs)〛in
    then ∃ rs'' 
       st. Success rs'' = cegis l p τ σ es rs χs
         ∧ l (τ + es) = p (σ +  rs'')
+——————————————————————————————————–+
Proceed by induction on |Packet² - χs|.


Base case. |χs| = |Packet²|.
  intros.
  let rs' be such that l τ = p (σ + es).
  ∀ (in, out) ∈ χs. 〚l τ〛in = out = 〚p (σ + rs)〛 in
  Since χs = Packet².
  ∀ (in, out) ∈ Packet². 〚l τ〛in = out = 〚p (σ + rs)〛in
  i.e.
  ∀ in ∈ Packet. 〚l τ〛in = 〚p (σ + rs)〛in
  i.e.
  l τ = p (σ + rs) is valid.
  and we're done.

Inductive step |χs| = n - 1.
  intros.
  let rs' be such that l τ = p (σ + rs').
  ∀ (in, out) ∈ χs. 〚l τ〛in = out = 〚p (σ + rs)〛in (assnχs)
  Case 1. l τ = p (σ + es) is valid, then we're done.
  Case 2.
     l τ = p (σ + es) produces a counterexample χ = (in,out).
     i.e. 〚l τ〛in = out ≠ 〚p (σ + es)〛in

     notice that because l τ = p (σ + rs'),
     ∀ (in, out) ∈ ({χ}∪χs),
         [|p (σ + rs')|] in = out.

     Then by the lemma, rs'' = get_model' p σ es ({̧{χ}∪χs)
     and ∀ (in, out) ∈ ({χ}∪χs),
           [|p (σ + rs'')|] in = out.

     By assnχs, |{χ} ∪ χs| = n, the result follows by IH.

□



################# Optimizations ###########################

—————————————————————
Fast counter-example.
————————————————————–

get_cex l p τ σ r es
= VALID(l (τ + [r]) == (p (τ  + es)))

get_cex l p τ σ r es
= match hits l τ r
  | None   → None
  | Some χ →
    if 〚l (τ + [r])〛χ = 〚p (σ + es)〛χ
    then None
    else Some χ


# if pkt ⊧ hits p l τ r es
# then pkt hits r
hits l τ r = wp(truncate l table(r),
                negate_matches(table(r),τ) ∧ mkMatch(r))

truncate t skip    = None
truncate t (f ← e) = None
truncate t (c₁;c₂) =
  match truncate t c₂ with
  | None → truncate t c₁
  | Some c₂' → c₁;c₂'
truncate t (s.apply()) | s = t     = Some skip
                       | otherwise = None
truncate t (if bcs fi) =
  if find_one (λ (b→c).
       match truncate t c with
       | Some c' → Some (Assume b c')
       | None -> None)
  fi


### Assume (l τ = p σ)
let cegis l p τ σ rs es χs =  
  match get_cex l p τ σ rs es with
  | None → Success es
  | Some χ →
     match get_model' p σ es ({χ}∪χs) with
     | unsat → fail
     | sat es' → cegis l p τ σ es' ({χ}∪χs)

—————————————————————————
Minimizing Deletion Holes
—————————————————————————

modify the holes function in get_model as follows:

holes(p, τ, χs)
#### for t in tables(p), for i ∈ [0,|χs|)  ?Addᵢ(t) ∈ holes(p,τ, χs)
#### for t ∈ tables(p), and 0 ≤ i < |τ(t)|,
####### ∃ (in,out) ∈ χs. nd-hits(in, p, τ, t, i)
####### ⇒ ?Del(t,i) ∈ holes(p,n,τ)

let get_model'' p σ r χs =
  let σ' = σ + holes(p, σ, χs) in
  SAT(⋀{in ⇒ wp(p σ', out) | (in,out) ∈ χs})

——————————————————————————————————————-+
Lemma.
∀ p  program,
  σ  instantiation,
  χs counterexamples.
if ∃ rs st
     ∀ (in,out) ∈ χs,
      〚p (σ + rs)〛 in = out
then ∃ rs' st.
       rs' = get_model'' p σ r χs
       ∧ ∀ (in,out) ∈ χs,
         〚p (σ + rs')〛in = out
———————————————————————————————————————+
intros *.

Let rs st ∀ (in,out) ∈ χs, 〚p (σ + rs)〛in = out (1)

Restrict σ + rs to only the entries hit by some counter example in χs.
Call this new instantiation σ'.

Consider the following model for holes(p, |χs|, σ):
  ds = all deletions succeed
  as = tables(p)
     >>| λ t → [ Add(t, σ'(t)[i]) | 0 ≤ i < |σ'(t)| ]

  rs' = ds + as

Notice that σ + rs' = σ + ds + as

None of the counter examples in χs will hit any row in σ + ds.
so  p (σ + ds + as) =χs= p (· + as) =χs= p σ'

Conclude (by 1,2) that ∀ (in,out) ∈ χs, 〚p (σ + rs')〛in = out
*================================*

+——————————————————————————+
| reducing concurrent CEXs |
+——————————————————————————+

let cegis l p τ σ es χs =
  match l τ = p (σ + es) with
  | Valid → Success es
  | Cex χ →
     if χ ∈ χs
     then match get_model' p σ es ({χ}∪χs) with
          | unsat → fail
          | sat es' → cegis l p τ σ es ({χ}∪χs)
     else match get_model p σ es χ with
          | unsat → fail
          | sat es' → cegis l p τ σ (es + es') ({χ}∪χs)

—–
either l τ =χs= p (σ + es)
or     ⋄(χ ∈ χs)

—–


+————————————————————————+
| Mutual recursion & DFS |
+————————————————————————+
  
let cegis l p τ σ es χs =
  match l τ = p (σ + es) with
  | Valid → success es
  | Cex χ → solve l p τ σ es χ χs ⊤  
and solve l p τ σ es χs φ =
  if φ is unsat
  then fail
  else
    match get_model'' l p τ σ es χ φ with
      | none → fail
      | some es' →
         match cegis l p τ σ (es·es') χs with
	 | fail → solve l p τ σ es χs (φ∧¬es')
	 | success es'' → success es''

let get_model'' p σ es χ@(pkt₀,pkt₁) φ =
  let σ' = σ + es + holes(p,1,σ) in
  SAT(φ ∧ (pkt₀ ⇒ wp(p σ', pkt₁)))


lexicographical measure (|Packet² ∖ χs|
                        , we get closer to the maximum extension of es very time?
			, |φ|)


We still need a recursion measure. There are an infinite number of
sequences of edits, but there is a finite sequence of reachable edits,
because once the entire domain of input packets is enumerated in a
table the disjoint choice semantics prevent further edits from being
reachable.

In fact, for a given program p and instance τ,
every
nonrepeating,
monotonic,
sequence of edits es,
has an index N > 0 such that
    p (σ + es[:N]) = p (σ + es)

Lemma. get_model'' always returns a not shadowed, monotonic edit.

Lemma. All sequences of nonrepeating edits are finite.

Lemma. Theres a finite number of solutions to every query.

Theorem (Completeness).

Proof. We explore all reachable sequences of edits.

Note that we can un-lock-in our rules so far by making a recursive call
to
  cegis l p τ      σ       (es · es') χs
  cegis l p τ (σ + es·es')     []     χs

If we do finitely many of these "freedom steps" we're still guaranteed
termination, so this would just be heuristic.

+—————————————————+
| Program Slicing |
+—————————————————+

sliceAdd(skip, τ, e) = skip
sliceAdd(f:=e, τ, e) = f:=e
sliceAdd(c₁;c₂, τ, e) = sliceAdd(c₁,τ,e);sliceAdd(c₂,τ,e)
sliceAdd(if bcs fi, τ, e) = if (λ b→c. b → sliceAdd c τ e) `map` bcs  fi
sliceAdd(t.apply(), τ, s, ρ)
  | name(t) = s
  = if negate_matches(τ,t) ∧ mkMatch(t,ρ)
        → ρ.action(ρ.action_data)
       true → t.default
    fi
  | otherwise
  = t.apply() τ


————————————————————————————
Theorem.

∀ l logical program,
  p physical program,
  τ logical instantiation,
  σ physical instantiation,
  e logical addition,
  r physical addition.

  if
    hits l τ e ⇔ hits p σ r
  and
    sliceAdd(l, τ, e) = sliceAdd(p, σ, r)
  and
    l τ = p σ
  then
    l (τ + e) = p (σ + r)
—————————————————————————————

intros.

(assn1) hits l τ e ⇔ hits p σ r
(assn2) sliceAdd(l, τ, e) = sliceAdd(p, σ, r)
(assn3) l τ = p σ

let pkt ∈ Packet be arbitrary.

Show that 〚l (τ + Add(e))〛pkt = 〚p (σ + Add(r))〛pkt.

Case 1 [pkt ⊧ hits l τ e].
   By assn1. pkt ⊧ hits p σ r
   By HitSliceAdd.  〚sliceAdd(l, τ, e)〛pkt = 〚l (τ + Add(e))〛pkt
   By HitSliceAdd). 〚sliceAdd(p, σ, r)〛pkt = 〚p (σ + Add(r))〛pkt
   Result follows by assn 2.

Case 2 [¬(pkt ⊧ hits l τ e)]
   By assn 1. ¬(pkt ⊧ hits p σ r)
   By HitSliceAdd. 〚l τ〛pkt = 〚l (τ + Add(e))〛pkt
   By HitSliceAdd. 〚p σ〛pkt = 〚p (σ + Add(r))〛pkt
   result follows by assn3
□
*=============================*


——————————————————————————————————————————————————————–
Lemma HitSliceAdd.

∀ l well-formed program,
  τ instantiation,
  r addition
  pkt Packet.

  if
    pkt ⊧ hits l τ r
  then
    〚sliceAdd(l, τ, r)〛 pkt = 〚l (τ + Add(r))〛 pkt
and
  if
    ¬pkt ⊧ hits l τ r
  then
    〚l τ〛 pkt = 〚l (τ + Add(r))〛 pkt
———————————————————————————————————————————————————————

Proceed by induction on the structure of l, leaving pkt general.

[skip]  sliceAdd(skip,τ,r) = skip = skip τ ✓
[f:=e]  sliceAdd(f:=e,τ,r) = f:=r = f:=r τ ✓
[c₁;c₂] let pkt ∈ Packet be arbitrary.
        Prove the first condition.
	  (assn1)  pkt ⊧ hits (c₁;c₂) τ r;
        then by lemma(??)
	  (assn1a) pkt ⊧ hits c₁ τ r XOR pkt ⊧ hits c₂ τ r.
          WLOG (assn2a) pkt ⊧ hits c₁ τ r;
 	       (assn2b) ¬pkt ⊧ hits c₂ τ r
            So compute
              〚c₁;c₂ (τ + Add(r))〛 pkt
               = 〚c₂ (τ + Add(r))〛(〚c₁ (τ + Add(r))〛pkt)
               = 〚c₂ τ〛(〚sliceAdd(c₁,τ,r)〛pkt) by IH(assn2a)IH(assn2b)
	       = 〚sliceAdd(c₂,τ,r)〛(〚sliceAdd(c₂,τ,r〛pkt)  by lemma(??)
	       = 〚sliceAdd(c₁,τ,r);sliceAdd(c₂,τ,r)〛pkt	       
	       = 〚sliceAdd(c₁;c₂,τ,r)〛pkt
	       ✓
	Prove the second condition
	  (assn1)  ¬(pkt ⊧ hits (c₁;c₂) τ r)
	then by lemma???
	  (assn1a) ¬(pkt ⊧ hits c₁ τ r)
	  (assn2b) ¬(pkt ⊧ hits c₂ τ r)

        So compute
         〚c₁;c₂ (τ + Add(r))〛 pkt
          = 〚c₂ (τ + Add(r))〛(〚c₁ (τ + Add(r))〛pkt)
	  = 〚c₂ τ〛(〚c₁ τ〛pkt) by IH(assn1a)IH(assn1b)
	  = 〚c₁;c₂ τ〛 pkt
	  ✓

[if bcs fi] let pkt ∈ Packet be arbitrary.
            Prove the first condition.
	        (assn1) pkt ⊧ hits (if bcs fi) τ r
	      then by lemma(??)
	         (assn2) ∃! 0 ≤ i < |bcs| st pkt ⊧ hits cᵢ τ r
	      Then by lemma(??)(assn1,assn2)
	         〚if bcs fi (τ + Add(r))〛pkt
	         = 〚cᵢ (τ + Add(r))〛pkt
	         = 〚sliceAdd(cᵢ, τ, r)〛pkt By IH
	         = 〚sliceAdd(if bcs fi, τ, r)] pkt by lemma(??,assn1)
	         ✓

            Prove the second condition.
	        (assn1) ¬(pkt ⊧ (if bcs fi) τ r
	      so by lemma(??, assn1)
	        (assn2) ∀ 0 ≤ i < |bcs|. ¬(pkt ⊧ hits cᵢ τ r)
              so by repeated IH
	        (assn3) ∀ 0 ≤ i < |bcs|. 〚cᵢ (τ + Add(r))〛pkt = 〚cᵢ τ〛pkt
	      so calculate
	      〚if bcs fi (τ + Add(r))〛pkt
	       = 〚(λ (b→c).b → c (τ + Add(r))) `map` bcs〛 pkt
	       = 〚(λ (b→c).b → c τ) `map` bcs〛 pkt  by assn3
	       = 〚if bcs fi τ〛pkt
	       ✓


[t.apply()] let pkt ∈ Packet be arbitrary.
	    Two cases
	    1. table(r) ≠ name(t)
               then by lemma(??)
                 t.apply() τ
		 = t.apply() (τ + Add(r))
		 = sliceAdd(t.apply(), τ, r)
	       ✓
	    2. table(r) = name(t)
               Then
                 A. t.apply() τ = if cases(τ,t) fi
		 B. t.apply() τ = if cases(τ + Add(r),t)
		 C. sliceAdd(t.apply(), τ,r)
		    = if negate_matches(τ,t) ∧ (mkMatch(t,ρ)
      		          → ρ.action(ρ.action_data)
			  true → t.default
                      fi
		 
               Prove the first condition.
	           (assn1) pkt ⊧ hits t.apply() τ r
	         then by lemma(??)
		   (assn2)〚t.apply (τ + Add(r))〛pkt = 〚r.action(r.action_data)〛pkt

 		 Then Calculate
		   hits t.apply() τ r
		   = wp(truncate t.apply() table(r),
                       negate_matches(table(r),τ) ∧ mkMatch(r))
		   = wp(truncate t.apply() t,
                       negate_matches(t,τ) ∧ mkMatch(r))
		   = wp(skip, negate_matches(t,τ) ∧ mkMatch(r))
		   = negate_matches(t, τ) ∧ mkMatch(r)

                 Calculate again that
		 〚sliceAdd(t.apply(), τ,r)〛pkt
                  = 〚if negate_matches(τ,t) ∧ mkMatch(r)
      		          → r.action(r.action_data)
			  true → t.default
                     fi〛pkt
		  = 〚r.action(r.action_data) fi〛pkt  since pkt ⊧ hits t.apply τ r

		  the goal is proved by the previous conclusion,
		  (assn2) and transitivity of equality ✓
		  
               Prove the second condition.
	           (assn1) ¬(pkt ⊧ hits t.apply() τ r)
                 Then by lemma(??, assn1) there are two cases:
		   Case 1 [∀ r' ∈ τ(t). ¬(pkt ⊧ hits τ r')].
		      Then by Lemma(??)
		        〚t.apply() τ〛pkt = 〚t.default〛 pkt
	              Observe also that
		        ∀ r' ∈ (τ + Add(r))(t). ¬(pkt ⊧ hits τ r')
		      So by the same Lemma,
		        〚t.apply() (τ + Add(r))〛pkt = 〚t.default〛 pkt
		      and we're done ✓
		      
		   Case 2. [∃ r' ∈ τ(r). pkt ⊧ hits τ r']
		     then by Lemma(??)
		       〚t.apply() τ〛pkt = 〚r'.action(r'.action_data)〛 pkt
		     Observe also that
		        ∃ r'' ∈ (τ + Add(r))(t). pkt ⊧ hits τ r''
			and r'' = r'
	             So then,
      		       〚t.apply() (τ + Add(r))〛pkt = 〚r'.action(r'.action_data)〛pkt
		     ✓
□
*====================================================================================*


		       
+-------------------------------------------+
|  Equivalence classes of edits             |
|         in a context p & τ                |
+-------------------------------------------+

Given a program p and an instantiation τ, we can think about
equivalence classes of edits -- this is useful when we want to bubble
up conclusions from recursive calls.

For instance, if I explore the wrong path, say

+———————————+
|  LOGICAL  |
+———————————+
| x#2 | y#2 |
+———————————+
| 0   | 0   |
| *   | nop |
+———————————+


We get to a point where we can synthesize this mofo:
+———————————+
| PHYSICAL  |
+———————————+
| x#2 | y#2 |
+———————————+
| 1   | 1   |
| 2   | 2   |
| 3   | 3   |
| 0   | 0   |
+———————————+

WE get a counter example:
     x = 1
     y = 2
and we can't handle it,

So, we'll conclude that
¬(?AddToPHYSICAL = 1
   ∧ ?x = 0 ∧ ?y = 1)

