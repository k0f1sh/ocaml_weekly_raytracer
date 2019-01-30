(* open Base *)
type t = {a: Vec3.t;
          b: Vec3.t}
         
let create a b = {a = a; b = b}
let origin r = r.a
let direction r = r.b
let point_at_parameter r t = Vec3.plus r.a (Vec3.mulf r.b t)


