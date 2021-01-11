type t =
  | True
  | False
  | Eq of (Expr.t * Expr.t)
  | Le of (Expr.t * Expr.t)
  | And of (t * t)
  | Or of (t * t)
  | Impl of (t * t)
  | Iff of (t * t)
  | Neg of t

(** [equals a b] is true iff [a] and [b] represent the same AST *)
val equals : t -> t -> bool

(** [to_string t] is a string representation of the test t*)
val to_string : t -> string

(** [to_sexp_string t] is a SExp string representation of the test t*)
val to_sexp_string : t -> string

(** [eq] is a smart constructor for [Eq] tests*)
val eq : Expr.t -> Expr.t -> t

(** [(%=%) = eq]*)
val (%=%) : Expr.t -> Expr.t -> t

(** [a %<>% b] constructs a test that is true when where [a] and [b] are not equal*)
val (%<>%) : Expr.t -> Expr.t -> t

(** [leq] is a smart constructor for [Leq] tests*)
val leq : Expr.t -> Expr.t -> t

(** [(%<=%) = leq]*)
val (%<=%) : Expr.t -> Expr.t -> t
val (%>=%) : Expr.t -> Expr.t -> t
val (%<%) : Expr.t -> Expr.t -> t
val (%>%) : Expr.t -> Expr.t -> t


(** [or_] is a smart constructor for [Or] tests*)
val or_ : t -> t -> t

(** [a %+% b = or_ a b] *)
val (%+%) : t -> t -> t

(** [bigor as] computes an [Or] of a list of tests *)
val bigor : t list -> t

(** [and_] is a smart constructor for [And] tests*)
val and_ : t -> t -> t

(** [a %&% b = and_ a b] *)
val (%&%) : t -> t -> t

(** [bigand as] computes an [and_] of a list of tests *)
val bigand : t list -> t

(** [neg] is a smart constructor for [Neg] tests *)
val neg : t -> t

(** [!% b = neg b] *)
val (!%) : t -> t


(** [impl a b] construct a term equivalent to [Impl(a,b)]. [a %=>% b] is its
   infix operator. *)
val impl : t -> t -> t
val (%=>%) : t -> t -> t

(** [iff a b] construct a term equivalent to [Iff(a,b)]. [a %<=>% b] is its
   infix operator. *)
val iff : t -> t -> t
val (%<=>%) : t -> t -> t

(** [num_nodes a] is the number of AST nodes in a *)
val num_nodes : t -> int

(** [frees typ a] computes the list of sized identifiers. If [typ = `Hole] it
   returns all [Hole]s. If [type = `Var] it returns all [Var]s. *)
val frees : [`Var | `Hole] -> t -> (string * int) list

(** [vars a] is a list ofall of the [Var]s in [a] *)
val vars : t -> (string * int) list

(** [Hole a] is a list of all of the [Hole]s in [a] *)
val holes : t -> (string * int) list

(** [has_hole a] is true iff [a] contains a [Hole] AST node*)
val has_hole : t -> bool

(** [multi_vals a] is the list of values that occur in [a]. The [multi_] prefix
   indicates that the list may contain duplicates *)
val multi_vals : t -> Value.t list

(** [holify f vs a] replaces each sized variable [(v,sz)] in [frees a ∩ vs] with
 ** [f (v,sz)]
 ** *)
val holify : f:((string * int) -> (string * int)) -> string list -> t -> t
