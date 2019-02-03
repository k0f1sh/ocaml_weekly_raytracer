open Base

type t
val create: float -> float -> t
val default_camera: t
val get_ray: t -> float -> float -> Ray.t

