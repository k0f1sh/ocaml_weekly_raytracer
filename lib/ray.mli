open Base

type t
val create: Vec3.t -> Vec3.t -> t
val origin: t -> Vec3.t
val direction: t -> Vec3.t
val point_at_parameter: t -> float -> Vec3.t


