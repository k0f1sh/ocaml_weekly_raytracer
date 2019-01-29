open Base

type t

val create: float -> float -> float -> t

val xyz: t -> float * float * float

val x: t -> float
val y: t -> float
val z: t -> float

val squared_length: t -> float
val length: t -> float
val make_unit_vector: t -> t
val (+): t -> t -> t
val (-): t -> t -> t
val ( * ): t -> t -> t
val (/): t -> t -> t
val (/.): t -> float -> t
val ( *. ): t -> float -> t
val dot: t -> t -> float
val cross: t -> t -> t
val unit_vector: t -> t

