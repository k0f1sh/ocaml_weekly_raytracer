open Base

type t = Sphere of Vec3.t * float

let sphere center radius = Sphere (center, radius)

let hit_sphere center radius r t_min t_max hit_record =
  let oc = Vec3.minus (Ray.origin r) center in
  let a = Vec3.dot (Ray.direction r) (Ray.direction r) in
  let b = Vec3.dot oc @@ Ray.direction r in
  let c = (Vec3.dot oc oc) -. (radius *. radius) in
  let discriminant = b *. b -. a *. c in
  if Caml.(>) discriminant 0.0 then
    let temp = ((-1.0 *. b) -. (Float.sqrt discriminant)) /. a in
    if ((Caml.(<) temp t_max) && (Caml.(>) temp t_min)) then
      Some (Hit_record.create
                temp
                (Ray.point_at_parameter r (Hit_record.tf hit_record))
                (Vec3.divf (Vec3.minus (Hit_record.p hit_record) center) radius))
    else
      None
  else
    let temp = ((-1.0 *. b) +. (Float.sqrt discriminant)) /. a in
    if ((Caml.(<) temp t_max) && (Caml.(>) temp t_min)) then
      Some (Hit_record.create
                temp
                (Ray.point_at_parameter r (Hit_record.tf hit_record))
                (Vec3.divf (Vec3.minus (Hit_record.p hit_record) center) radius))
    else
      None

let hit hitable r t_min t_max hit_record =
  match hitable with
    Sphere (center, radius) -> hit_sphere center radius r t_min t_max hit_record

(* TODO *)
(* module Sphere : sig
 *   type t
 *   val create : pos -> radius -> t
 *   val hit : ...
 * end *)
(* こんなのがいくつか出来上がる *)

(* hit、第一引数を除くと元のオブジェクトの型は消えるので *)
(* type scene = Ray.t -> float -> float -> Hit_record.t option *)
(* みたいに考えられるのではないか *)
(* このsceneで済むならそれを引き回す....足りなかったらレコード作って渡すかファンクタ使うか... *)



