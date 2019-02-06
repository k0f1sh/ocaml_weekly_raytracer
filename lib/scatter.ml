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
  Some (Some scatterd, attenuation)

let scatter_metal albedo fuzz r_in hit_record =
  let reflected = Vec3.reflect (Vec3.unit_vector (Ray.direction r_in))
                               (Hit_record.normal hit_record) in
  let scatterd = Ray.create (Hit_record.p hit_record)
                            (Vec3.plus reflected
                                       (Vec3.mulf (random_in_unit_sphere ()) fuzz)) in
  let attenuation = albedo in
  if Caml.(>) (Vec3.dot (Ray.direction scatterd) (Hit_record.normal hit_record)) 0.0 then
    Some (Some scatterd, attenuation)
  else
    None

let schlick cosine ref_idx =
  let r0 = (1.0 -. ref_idx) /. (1.0 +. ref_idx) in
  let r0 = r0 *. r0 in
  let c = 1.0 -. cosine in
  r0 +. (1.0 -. r0) *. (c *. c *. c *. c *. c)

let scatter_dielectric_outward_normal_and_ni_over_t ref_idx r_in hit_record =
    if Caml.(>) (Vec3.dot (Ray.direction r_in) (Hit_record.normal hit_record)) 0.0 then
      let outward_normal = Vec3.mulf (Hit_record.normal hit_record) (-1.0) in
      let cosine = ref_idx *. (Vec3.dot (Ray.direction r_in) (Hit_record.normal hit_record)) /. (Vec3.length (Ray.direction r_in)) in
      (outward_normal, ref_idx, cosine)
    else
      let outward_normal = Hit_record.normal hit_record in
      let ni_over_nt = 1.0 /. ref_idx in
      let cosine = (/.)
                     ((-1.0) *. (Vec3.dot (Ray.direction r_in) (Hit_record.normal hit_record)))
                     (Vec3.length (Ray.direction r_in)) in
      (outward_normal, ni_over_nt, cosine)

let scatter_dielectric ref_idx r_in hit_record =
  let d = (Ray.direction r_in) in
  let reflected = Vec3.reflect d (Hit_record.normal hit_record) in
  let (outward_normal, ni_over_nt, cosine) = scatter_dielectric_outward_normal_and_ni_over_t ref_idx r_in hit_record in
  match (Vec3.refract d outward_normal ni_over_nt) with
    Some (refracted) ->
     let reflect_probe = schlick cosine ref_idx in
     if Caml.(<) (Random.float 1.0) reflect_probe then
       Some (Some (Ray.create (Hit_record.p hit_record) reflected), Vec3.create 1.0 1.0 1.0)
     else
       Some (Some (Ray.create (Hit_record.p hit_record) refracted), Vec3.create 1.0 1.0 1.0)
  | None ->
     Some (Some (Ray.create (Hit_record.p hit_record) reflected), Vec3.create 1.0 1.0 1.0)

(* let scatter_emission r_in hit_record = *)
let scatter_emission albedo strength =
  Some (None, Vec3.mulf albedo strength)
        
let fn material r_in hit_record =
  match material with
    Material.Lambertian(albedo) -> scatter_lambertian albedo hit_record
  | Material.Metal(albedo, fuzz) -> scatter_metal albedo fuzz r_in hit_record
  | Material.Dielectric(ref_idx) -> scatter_dielectric ref_idx r_in hit_record
  | Material.Emission(albedo, strength) -> scatter_emission albedo strength

                                 
