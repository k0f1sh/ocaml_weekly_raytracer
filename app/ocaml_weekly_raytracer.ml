open Base
module Out = Stdio.Out_channel
module Vec3 = Raylib.Vec3
module Ray = Raylib.Ray
module Hitable = Raylib.Hitable
module Hit_record = Raylib.Hit_record

let nx = 200
let ny = 100
let output_file_name = "output.ppm"

let color r hitable =
  match (Hitable.hit hitable r 0.0 Float.max_finite_value) with
    Some hit_record ->
     Vec3.mulf (Vec3.plus (Vec3.create 1.0 1.0 1.0) (Hit_record.normal hit_record)) 0.5
  | None ->
     let unit_direction = Vec3.unit_vector (Ray.direction r) in
     let t = 0.5 *. ((Vec3.y unit_direction) +. 1.0) in
     let invert_t = 1.0 -. t in
     Vec3.plus (Vec3.mulf (Vec3.create 1.0 1.0 1.0) invert_t)
               (Vec3.mulf (Vec3.create 0.5 0.7 1.0) t)

let get_ray origin lower_left_corner horizontal vertical u v =
  Ray.create origin @@ Vec3.plus (Vec3.plus lower_left_corner (Vec3.mulf horizontal u)) (Vec3.mulf vertical v)

let scene = Hitable.of_list [
                Hitable.sphere (Vec3.create 0.0 0.0 (-1.0)) 0.5;
                Hitable.sphere (Vec3.create 0.0 (-100.5) (-1.0)) 100.0;
]

(* 1ピクセル分を描画する *)
(* Out_channel.t -> int -> int -> unit *)
let render chan x y =
  let lower_left_corner = Vec3.create (-2.0) (-1.0) (-1.0) in
  let horizontal = Vec3.create 4.0 0.0 0.0 in
  let vertical = Vec3.create 0.0 2.0 0.0 in
  let origin = Vec3.create 0.0 0.0 0.0 in
  let u = (Int.to_float x) /. (Int.to_float nx) in
  let v = (Int.to_float y) /. (Int.to_float ny) in
  let r = get_ray origin lower_left_corner horizontal vertical u v in
  let col = color r scene in
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
   

