open Base

type t = Sphere of Vec3.t * float * Material.t
       | Polygon of Vec3.t * Vec3.t * Vec3.t * Material.t
       | Hitable_list of t list

let polygon v1 v2 v3 material = Polygon (v1, v2, v3, material)
let sphere center radius material = Sphere (center, radius, material)
let of_list hitables = Hitable_list hitables

(* 反時計回り *)
let square v1 v2 v3 v4 material =
  of_list [
      polygon v1 v2 v3 material;
      polygon v1 v3 v4 material;
    ]


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

let hit_polygon material v1 v2 v3 r t_min t_max =
  let normal = Vec3.normal v1 v2 v3 in
  let origin = Ray.origin r in
  let direction = Ray.direction r in
  let xp = Vec3.minus v1 origin in
  let xpn = Vec3.dot xp normal in
  let vn = Vec3.dot direction normal in
  if Caml.(>=) 0.00001 vn then
    None
  else
    let t = xpn /. vn in
    if not ((Caml.(<) t t_max) && (Caml.(>) t t_min)) then
      None
    else
      let p = Ray.point_at_parameter r t in
      if Caml.(<) (Vec3.dot (Vec3.cross (Vec3.minus p v1) (Vec3.minus v2 v1)) normal) 0.0 then
        None
      else
        if Caml.(<) (Vec3.dot (Vec3.cross (Vec3.minus p v2) (Vec3.minus v3 v2)) normal) 0.0 then
          None
        else
          if Caml.(<) (Vec3.dot (Vec3.cross (Vec3.minus p v3) (Vec3.minus v1 v3)) normal) 0.0 then
            None
          else
            Some (Hit_record.create t p (Vec3.mulf normal (-1.0)) material)
            
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
  | Polygon (v1, v2, v3, material) -> hit_polygon material v1 v2 v3 r t_min t_max
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



