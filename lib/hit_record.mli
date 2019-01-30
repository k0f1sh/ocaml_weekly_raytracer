type t

val create: float -> Vec3.t -> Vec3.t -> t
val tf: t -> float
val p: t -> Vec3.t
val normal: t -> Vec3.t

