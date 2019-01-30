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
      begin
        HitRecord.update hit_record temp (Ray.point_at_parameter r (HitRecord.tf hit_record)) (Vec3.divf (Vec3.minus (HitRecord.p hit_record) center) radius);
        true
      end
    else
      false
  else
    let temp = ((-1.0 *. b) +. (Float.sqrt discriminant)) /. a in
    if ((Caml.(<) temp t_max) && (Caml.(>) temp t_min)) then
      begin
        HitRecord.update hit_record temp (Ray.point_at_parameter r (HitRecord.tf hit_record)) (Vec3.divf (Vec3.minus (HitRecord.p hit_record) center) radius);
        true
      end
    else
      false
    
let hit hitable r t_min t_max hit_record =
  match hitable with
    Sphere (center, radius) -> hit_sphere center radius r t_min t_max hit_record

