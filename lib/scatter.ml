open Base

let rec random_in_unit_sphere () =
  let p = (Vec3.minus
             (Vec3.mulf
                (Vec3.create (Random.float 1.0) (Random.float 1.0) (Random.float 1.0))
                2.0)
             (Vec3.create 1.0 1.0 1.0)) in
  if Caml.(>=) (Vec3.squared_length p) 1.0 then
    random_in_unit_sphere ()
  else
    p


let scatter_lambertian albedo hit_record =
  let target = Vec3.plus (Vec3.plus (Hit_record.p hit_record)
                                    (Hit_record.normal hit_record))
                         (random_in_unit_sphere ()) in
  let scatterd = Ray.create (Hit_record.p hit_record) (Vec3.minus target (Hit_record.p hit_record)) in
  let attenuation = albedo in
  Some (scatterd, attenuation)

let scatter_metal albedo r_in hit_record =
  let reflected = Vec3.reflect (Vec3.unit_vector (Ray.direction r_in))
                               (Hit_record.normal hit_record) in
  let scatterd = Ray.create (Hit_record.p hit_record) reflected in
  let attenuation = albedo in
  if Caml.(>) (Vec3.dot (Ray.direction scatterd) (Hit_record.normal hit_record)) 0.0 then
    Some (scatterd, attenuation)
  else
    None
    
let fn material r_in hit_record =
  match material with
    Material.Lambertian(albedo) -> scatter_lambertian albedo hit_record
  | Material.Metal(albedo) -> scatter_metal albedo r_in hit_record

                                 
