open Base
module Out = Stdio.Out_channel
module Vec3 = Raylib.Vec3
module Ray = Raylib.Ray

let nx = 200
let ny = 100
let output_file_name = "output.ppm"

let hit_sphere center radius r =
  let oc = Vec3.minus (Ray.origin r) center in
  let a = Vec3.dot (Ray.direction r) (Ray.direction r) in
  let b = 2.0 *. (Vec3.dot oc @@ Ray.direction r) in
  let c = (Vec3.dot oc oc) -. radius *. radius in
  let discriminant = b *. b -. 4.0 *. a *. c in
  if Caml.(<) discriminant 0.0 then
    -1.0
  else
    ((-1.0 *. b) -. (Float.sqrt discriminant)) /. (2.0 *. a)

let color r =
  let t = hit_sphere (Vec3.create 0.0 0.0 (-1.0)) 0.5 r in
  if Caml.(>) t 0.0 then
    let n = Vec3.unit_vector @@
              Vec3.minus
                  (Ray.point_at_parameter r t)
                  (Vec3.create 0.0 0.0 (-1.0)) in
    Vec3.mulf (Vec3.plus (Vec3.create 1.0 1.0 1.0) n) 0.5
  else
    let unit_direction = Vec3.unit_vector (Ray.direction r) in
    let t = 0.5 *. ((Vec3.y unit_direction) +. 1.0) in
    let invert_t = 1.0 -. t in
    Vec3.plus (Vec3.mulf (Vec3.create 1.0 1.0 1.0) invert_t)    
              (Vec3.mulf (Vec3.create 0.5 0.7 1.0) t)

let get_ray origin lower_left_corner horizontal vertical u v =
  Ray.create origin @@ Vec3.plus (Vec3.plus lower_left_corner (Vec3.mulf horizontal u)) (Vec3.mulf vertical v)

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
  let col = color r in
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
   


