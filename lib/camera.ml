open Base

type t = {
    lens_radius: float;
    origin: Vec3.t;
    lower_left_corner: Vec3.t;
    horizontal: Vec3.t;
    vertical: Vec3.t;
    u: Vec3.t;
    v: Vec3.t;
    w: Vec3.t;
  }

let rec random_in_unit_disk () =
  let p = (Vec3.minus (Vec3.mulf
                         (Vec3.create (Random.float 1.0) (Random.float 1.0) 0.0)
                         2.0)
                      (Vec3.create 1.0 1.0 0.0)) in
  if Caml.(>=) (Vec3.dot p p) 1.0 then
    random_in_unit_disk ()
  else
    p


let _create lookfrom lookat vup vfov aspect aperture focus_dist =
  let lens_radius = aperture /. 2.0 in
  let theta = (vfov *. Float.pi) /. 180.0 in
  let half_height = Float.tan (theta /. 2.0) in
  let half_width = aspect *. half_height in
  let origin = lookfrom in
  let w = Vec3.unit_vector (Vec3.minus lookfrom lookat) in
  let u = Vec3.unit_vector (Vec3.cross vup w) in
  let v = Vec3.cross w u in
  let lower_left_corner = (Vec3.minus
                             (Vec3.minus
                                (Vec3.minus origin (Vec3.mulf u (half_width *. focus_dist)))
                                (Vec3.mulf v (half_height *. focus_dist)))
                             (Vec3.mulf w focus_dist)) in
  let horizontal = Vec3.mulf u (2.0 *. half_width *. focus_dist) in
  let vertical = Vec3.mulf v (2.0 *. half_height *. focus_dist) in
  { lens_radius; origin; lower_left_corner; horizontal; vertical; u; v; w; }

let create x y =
  let lookfrom = (Vec3.create 0.0 1.0 (-4.0)) in
  let lookat = (Vec3.create 0.0 1.0 0.0) in
  _create
    lookfrom
    lookat
    (Vec3.create 0.0 1.0 0.0)
    50.0
    ((Int.to_float x) /. (Int.to_float y))
    0.001
    3.0

let get_ray camera s t =
  let rd = Vec3.mulf (random_in_unit_disk ()) camera.lens_radius in
  let offset = (Vec3.plus
                  (Vec3.mulf camera.u (Vec3.x rd))
                  (Vec3.mulf camera.v (Vec3.y rd))) in
  Ray.create (Vec3.plus camera.origin offset)
             (Vec3.minus (Vec3.minus (Vec3.plus
                                        (Vec3.plus camera.lower_left_corner
                                                   (Vec3.mulf camera.horizontal s))
                                        (Vec3.mulf camera.vertical t))
                                     camera.origin)
                         offset)
  
