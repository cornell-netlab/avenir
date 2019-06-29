# Hybrid Networks via Synthesis

We want to provide a way to program a network that is comprised of
both programmable and non-programmable devices, without the programmer
knowing which devices are programmable or not. The approach described
here will use [counter-example guided inductive
synthesis(CEGIS)](https://people.csail.mit.edu/asolar/papers/Solar-Lezama09.pdf).

We model a _Logical_ network program using GCL. Similarly, to model
the behavior of the real network, we assume that there is some _Real_
network program (written in GCL) that describes the behavior of the
non-programmable nodes in the network.

Similar to how Netkat models networking programs works, the _Logical_ and
_Real_ programs will take the following form:

```
Nat loc
Nat pkt where 0 <= pkt < 2^b    # b is the maximum length of a packet

loc in Ingress                  # Ingress is ingress locations in network
while(loc not in Egress) {      # Egress is ingress locations in network
  ...
  (X -> Y) 
  ...
}
```

The body of the while loop is a big _sequential_ select statement of
terms `X -> Y`, which are programs that describe the forwarding
behavior of the network. For instance `(loc = 1) /\ (pkt = 100) ->
(loc := 2 ) /\ (pkt = 2)` forwards packets equal to `100` at location
`1` to location `2` and updates the value of the packet to `200`.

### The Guarded Command Logic



### A Too-Simple Approach

With our logical and real programs in hand (`Pl` and `Pr`
respectively), we can formulate a weak feasibility problem:

> _Can `Pl` be implemented in `Pr`?_

The answer is dependent on the satisfiability of the following equation:

```
wp(Pl, ϕ) -> wp(Pr, ϕ)     (equation 1)
```

where `ϕ` is some correctness condition, expressing correctness
conditions such as "always forwards through a firewall", or more
commonly, "true"; and `wp` is the standard "weakest precondition" of
the formula.

Let's do a brief example.

_Example_ Say a programmer wants to run the following logical program.
```
loc = 0
while  loc != 2 {
  loc=1 ∧ pkt = 10.0.0.1 → loc := 2 ∧ pkt = 10.0.0.2
  true -> abort
}
```

The network can be described using a _real_ program
```
loc = 0
while loc != 2 {
  loc=0 → ?   # the hole represents the programmable switch
  loc=1 ∧ pkt = 10.0.0.1 → loc := 2 ∧ pkt = 10.0.0.2
  loc=1 ∧ pkt = 10.0.0.2 → loc := 2 ∧ pkt = 10.0.0.1
  * -> abort
}
```

Notice that on every non-aborting input, the second program behaves
like the first. However, it is not the case, yet that the second
program subsumes the first, since we have all of the packets that are
either not at location 1 or are not equal to `10.0.0.1` that must be
aborted as specified by the application programmer. So, somehow, we
need to synthesize a modification to the second program that ensures
that all packets matching `loc = 1 ∧ pkt = 10.0.0.2` are aborted.

For example:
```
loc = 1
while loc != 2 {
  ( # unalienable
    loc=1 ∧ pkt = 10.0.0.1 → loc := 2 ∧ pkt = 10.0.0.2
    loc=1 ∧ pkt = 10.0.0.2 → loc := 2 ∧ pkt = 10.0.0.1
	loc=0 ∧ pkt = 10.0.0.2 → abort
    true → abort
  )
}
```

## Synthesizing Modifications

To generalize our approach, will will use CEGIS to synthesize the
modifications necessary to ensure that `wp(P_l, ϕ) -> wp (f(P_r), ϕ)`.

_Problem 1. Can we find an modification `f : GCL → GCL` to `P_r` such
that `wp(P_l, ϕ) -> wp (P_r, ϕ)`._

In the above problem, we constrain our function `f` so that it cannot
modify the parts of `P_l` that describe the physical reality of the
*real* network. We call these portions of the code _unalienable_.

To use CEGIS, we must solve a finite subproblem of Problem 1: 

*Sub-Problem 1a. Given a finite set of traces `{tr₁, tr₂, …,tr₃}`
through `P_l`, and a predicate `T` describing that set of traces, can
we find a modification `f` such that `(wp(P_l, ϕ) /\ T) → wp(f(P_r), ϕ)`*

Out solution will be to use an SMT solver. We will instrument the real
program with "holes" for each programmable switch to allow. Instead of
a massive hole `?` like we have above in the real program, we will
provide some structure, e.g.

```
loc = 0 ∧ pkt = ?ₘ → loc := ?ₛ ∧ pkt := ?ₐ
```

Now, the SMT solver will give a set of conditions (e.g. on the match
hole `?ₘ`, the next-switch hole `?ₛ` and the action hole `?ₐ`) that
will affirm *Sub-Problem 1a*.

More specifically, we will sample a random packet from a uniform
distribution (`pᵢ ∼ Unif(Nat < 2ᵇ)`) and send it through the network
from one of the ingress hosts `Hᵢ`. Assume that when run through `P_l`
the packet reaches egress host `Hₒ` with value `pₒ` via trace `σ`.

Now we can perform symbolic execution on `P_r`.  Given a matching
relation on traces (such as equality, bisimilarity, or homoemorphic)
`R`, Pick a path `τ` such that `σ R τ`. Now we can compute `wp(τ, pkt
= pₒ).` Recall that we have inserted holes for the programmable
switches, so the formula `wp(τ, pkt = pₒ)` is expressed in terms of
these holes. Now the SMT solver will give us a model for these holes
to satisfy the subproblem.  Filling this model in to `P_r` gives us a
new program `S`. Now we know that `S` executed on `pᵢ` at `Hᵢ` will
give packet `pₒ` at `Hₒ`.






