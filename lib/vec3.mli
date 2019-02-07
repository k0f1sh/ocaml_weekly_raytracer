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
val plus: t -> t -> t
val minus: t -> t -> t
val mul: t -> t -> t
val div: t -> t -> t
val divf: t -> float -> t
val mulf: t -> float -> t
val dot: t -> t -> float
val cross: t -> t -> t
val unit_vector: t -> t
val reflect: t -> t -> t
val refract: t -> t -> float -> t option
val normal: t -> t -> t -> t
