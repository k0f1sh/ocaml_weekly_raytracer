open Base

type t = {
    origin: Vec3.t;
    lower_left_corner: Vec3.t;
    horizontal: Vec3.t;
    vertical: Vec3.t
  }

let create vfov aspect =
  let theta = vfov *. Float.pi /. 180.0 in
  let half_height = Float.tan @@ theta /. 2.0 in
  let half_width = aspect *. half_height in
  {
    origin = Vec3.create 0.0 0.0 0.0;
    lower_left_corner = Vec3.create ((-1.0) *. half_width) ((-1.0) *. half_height) (-1.0);
    horizontal = Vec3.create (2.0 *. half_width) 0.0 0.0;
    vertical = Vec3.create 0.0 (2.0 *. half_height) 0.0;
  }

let default_camera = create 120.0 2.0

let get_ray camera u v =
  Ray.create camera.origin (Vec3.minus (Vec3.plus
                                          (Vec3.plus camera.lower_left_corner
                                                     (Vec3.mulf camera.horizontal u))
                                          (Vec3.mulf camera.vertical v))
                                          camera.origin)

