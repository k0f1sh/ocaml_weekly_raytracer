open Base

type t
val sphere: Vec3.t -> float -> Material.t -> t
val of_list: t list -> t

val hit: t -> Ray.t -> float -> float -> Hit_record.t option

