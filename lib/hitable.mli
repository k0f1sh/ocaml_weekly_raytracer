open Base

type t
val sphere: Vec3.t -> float -> Material.t -> t
val of_list: t list -> t
val polygon: Vec3.t -> Vec3.t -> Vec3.t -> Material.t -> t

(* 反時計回り *)
val square: Vec3.t -> Vec3.t -> Vec3.t -> Vec3.t -> Material.t -> t

val hit: t -> Ray.t -> float -> float -> Hit_record.t option

