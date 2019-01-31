open Base
module Out = Stdio.Out_channel
module Vec3 = Raylib.Vec3
module Ray = Raylib.Ray
module Hitable = Raylib.Hitable
module Hit_record = Raylib.Hit_record
module Camera = Raylib.Camera

let nx = 200
let ny = 100
let ns = 100
let output_file_name = "output.ppm"

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
  
let rec color r hitable =
  match (Hitable.hit hitable r 0.0 Float.max_finite_value) with
    Some hit_record ->
     let target = (Vec3.plus (Vec3.plus (Hit_record.p hit_record)
                                        (Hit_record.normal hit_record))
                             (random_in_unit_sphere ())) in
     Vec3.mulf (color (Ray.create (Hit_record.p hit_record) (Vec3.minus target (Hit_record.p hit_record))) hitable) 0.5
  | None ->
     let unit_direction = Vec3.unit_vector (Ray.direction r) in
     let t = 0.5 *. ((Vec3.y unit_direction) +. 1.0) in
     let invert_t = 1.0 -. t in
     Vec3.plus (Vec3.mulf (Vec3.create 1.0 1.0 1.0) invert_t)
               (Vec3.mulf (Vec3.create 0.5 0.7 1.0) t)

let scene = Hitable.of_list [
                Hitable.sphere (Vec3.create 0.0 0.0 (-1.0)) 0.5;
                Hitable.sphere (Vec3.create 0.0 (-100.5) (-1.0)) 100.0;
]

(* サンプリング1回 *)
let sample u v =
  let r = Camera.get_ray Camera.default_camera u v in
  color r scene

(* サンプリングns回 *)
let sample_ns x y =
  let col = List.fold (List.range 0 100) ~init:(Vec3.create 0.0 0.0 0.0)
            ~f:(fun col _ ->
              let u = ((Int.to_float x) +. Random.float 1.0) /. (Int.to_float nx) in
              let v = ((Int.to_float y) +. Random.float 1.0) /. (Int.to_float ny) in
              Vec3.plus col @@ sample u v) in
  let col = Vec3.divf col @@ Int.to_float ns in
  let (x, y, z) = Vec3.xyz col in
  Vec3.create (Float.sqrt x) (Float.sqrt y) (Float.sqrt z)

(* 1ピクセル分を描画する *)
(* Out_channel.t -> int -> int -> unit *)
let render chan x y =
  let col = sample_ns x y in
  let (r, g, b) = Vec3.xyz col in
  begin
    Out.output_string
      chan
      ((Int.to_string (Float.to_int (255.99 *. r))) ^ " " ^
         (Int.to_string (Float.to_int (255.99 *. g))) ^ " " ^
           (Int.to_string (Float.to_int (255.99 *. b))));
    Out.newline chan;
  end


(* MAIN *)
let () =
  let chan = Out.create output_file_name
  in begin
      Out.print_endline "start...";
      Out.output_string chan ("P3\n" ^ (Int.to_string nx) ^ " " ^ (Int.to_string ny) ^ "\n255\n");
      List.iter (List.rev (List.range 0 ny))
                ~f:(fun y ->
                  List.iter (List.range 0 nx)
                            ~f:(fun x -> render chan x y));
      Out.close chan;
      Out.print_endline "complete!";
    end
   

