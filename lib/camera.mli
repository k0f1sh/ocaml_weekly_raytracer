open Base

type t
val create: Vec3.t -> Vec3.t -> Vec3.t -> Vec3.t -> t
val default_camera: t
val get_ray: t -> float -> float -> Ray.t

