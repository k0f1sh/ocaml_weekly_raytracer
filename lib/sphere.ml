open Base

(* TODO Hitable.mliとかに置きたい *)
module type Hitable = sig
  val hit: Ray.t -> float -> float -> HitRecord.t -> bool
end

(* sphereには center と radiusが必要。 *)
(* 型パラメータで必要な型を渡せるように? *)
(* SphereとかPolygonとか *)

module SPHERE: Hitable = struct
  let hit _ _ _ _ = true
  (* let hit r t_min t_max reco = true *)
end
