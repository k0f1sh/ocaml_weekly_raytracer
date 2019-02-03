open Base

type t = {
    origin: Vec3.t;
    lower_left_corner: Vec3.t;
    horizontal: Vec3.t;
    vertical: Vec3.t
  }

let _create lookfrom lookat vup vfov aspect =
  let theta = (vfov *. Float.pi) /. 180.0 in
  let half_height = Float.tan (theta /. 2.0) in
  let half_width = aspect *. half_height in
  let origin = lookfrom in
  let w = Vec3.unit_vector (Vec3.minus lookfrom lookat) in
  let u = Vec3.unit_vector (Vec3.cross vup w) in
  let v = Vec3.cross w u in
  let lower_left_corner = (Vec3.minus
                             (Vec3.minus
                                (Vec3.minus origin (Vec3.mulf u half_width))
                                (Vec3.mulf v half_height))
                             w) in
  let horizontal = Vec3.mulf u (2.0 *. half_width) in
  let vertical = Vec3.mulf v (2.0 *. half_height) in
  { origin; lower_left_corner; horizontal; vertical }

let create x y = _create
                   (Vec3.create (-2.0) 2.0 1.0)
                   (Vec3.create 0.0 0.0 (-1.0))
                   (Vec3.create 0.0 1.0 0.0)
                   90.0
                   ((Int.to_float x) /. (Int.to_float y))

let get_ray camera u v =
  Ray.create camera.origin (Vec3.minus (Vec3.plus
                                          (Vec3.plus camera.lower_left_corner
                                                     (Vec3.mulf camera.horizontal u))
                                          (Vec3.mulf camera.vertical v))
                                       camera.origin)

