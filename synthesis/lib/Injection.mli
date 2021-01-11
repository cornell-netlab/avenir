type t

val make : Problem.t -> t
val apply : t -> Test.t -> Test.t
val optimization : Parameters.t -> Problem.t -> Test.t -> Test.t
