type t = { origin: Vec3.t;
           lower_left_corner: Vec3.t;
           horizontal: Vec3.t;
           vertical: Vec3.t
         }

let create origin lower_left_corner horizontal vertical = { origin;
                                                            lower_left_corner;
                                                            horizontal;
                                                            vertical
                                                          }

let default_camera = create
                       (Vec3.create 0.0 0.0 0.0)
                       (Vec3.create (-2.0) (-1.0) (-1.0))
                       (Vec3.create 4.0 0.0 0.0)
                       (Vec3.create 0.0 2.0 0.0)

let get_ray camera u v =
  Ray.create camera.origin (Vec3.minus (Vec3.plus
                                          (Vec3.plus camera.lower_left_corner
                                                     (Vec3.mulf camera.horizontal u))
                                          (Vec3.mulf camera.vertical v))
                                          camera.origin)

