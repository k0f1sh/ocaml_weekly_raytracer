open Base
module Vec3 = Raylib.Vec3
module Ray = Raylib.Ray

module type TYPE = sig
  type t
  val tf: t -> float
  val p: t -> Vec3.t
  val normal: t -> Vec3.t
  val hit: Ray.t -> float -> float -> t -> bool
end

