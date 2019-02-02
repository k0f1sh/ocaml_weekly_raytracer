open Base

type t = Sphere of Vec3.t * float * Material.t
       | Hitable_list of t list

let sphere center radius material = Sphere (center, radius, material)
let of_list hitables = Hitable_list hitables

let hit_sphere material center radius r t_min t_max =
  let oc = Vec3.minus (Ray.origin r) center in
  let a = Vec3.dot (Ray.direction r) (Ray.direction r) in
  let b = Vec3.dot oc @@ Ray.direction r in
  let c = (Vec3.dot oc oc) -. (radius *. radius) in
  let discriminant = b *. b -. a *. c in
  if Caml.(>) discriminant 0.0 then
    let temp = ((-1.0 *. b) -. (Float.sqrt discriminant)) /. a in
    if ((Caml.(<) temp t_max) && (Caml.(>) temp t_min)) then
      let p = (Ray.point_at_parameter r temp) in
      Some (Hit_record.create
              temp
              p
              (Vec3.divf (Vec3.minus p center) radius)
              material)
    else
      None
  else
    let temp = ((-1.0 *. b) +. (Float.sqrt discriminant)) /. a in
    if ((Caml.(<) temp t_max) && (Caml.(>) temp t_min)) then
      let p = (Ray.point_at_parameter r temp) in
      Some (Hit_record.create
              temp
              p
              (Vec3.divf (Vec3.minus p center) radius)
              material)
    else
      None

let rec hit_hitable_list_sub hitable_list r t_min hit_record =
  match hitable_list with
    [] -> hit_record
  | first :: rest ->
     match (hit first r t_min (Hit_record.tf hit_record)) with
       Some new_hit_record -> hit_hitable_list_sub rest r t_min new_hit_record
     | None -> hit_hitable_list_sub rest r t_min hit_record
and hit_hitable_list hitable_list r t_min t_max =
  match hitable_list with
    [] -> None
  | first :: rest ->
     match hit first r t_min t_max with
       Some new_hit_record -> Some (hit_hitable_list_sub rest r t_min new_hit_record)
     | None -> hit_hitable_list rest r t_min t_max
and hit hitable r t_min t_max =
  match hitable with
    Sphere (center, radius, material) -> hit_sphere material center radius r t_min t_max
  | Hitable_list hitable_list -> hit_hitable_list hitable_list r t_min t_max

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



