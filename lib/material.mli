(* open Base *)

(* 中身は公開 *)
type t = Lambertian of Vec3.t (* albedo *)
       | Metal of Vec3.t * float (* albedo * fuzz *)
       | Dielectric of float
       | Emission of Vec3.t * float (* albedo * strength *)


