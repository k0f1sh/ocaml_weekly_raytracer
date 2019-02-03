open Base

type t = Lambertian of Vec3.t (* albedo *)
       | Metal of Vec3.t * float (* albedo * fuzz *)
       | Dielectric of float


