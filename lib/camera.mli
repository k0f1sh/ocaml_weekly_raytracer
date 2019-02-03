open Base

type t
val create: int -> int -> t
val get_ray: t -> float -> float -> Ray.t

