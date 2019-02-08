open Base
module Out = Stdio.Out_channel
module Vec3 = Raylib.Vec3
module Ray = Raylib.Ray
module Hitable = Raylib.Hitable
module Hit_record = Raylib.Hit_record
module Camera = Raylib.Camera
module Scatter = Raylib.Scatter
module Material = Raylib.Material

let nx = 500
let ny = 500
let ns = 500
let max_depth = 5
let output_file_name = "output.ppm"

let rec color r hitable depth =
  match (Hitable.hit hitable r 0.001 Float.max_finite_value) with
    Some hit_record ->
     if (Caml.(<) depth max_depth) then
       (* Ray.t * Vec3.t *)
       let resultopt = (Scatter.fn
                          (Hit_record.material hit_record)
                          r
                          hit_record) in
       match resultopt with
         Some (scatterd_opt, attenuation) -> begin
           match scatterd_opt with
             Some (scatterd) -> Vec3.mul (color scatterd hitable (depth + 1)) attenuation
           | None -> attenuation
         end
       | None -> Vec3.create 0.0 0.0 0.0
     else
       Vec3.create 0.0 0.0 0.0
  | None ->
     Vec3.create 0.0 0.0 0.0
     (* let unit_direction = Vec3.unit_vector (Ray.direction r) in
      * let t = 0.5 *. ((Vec3.y unit_direction) +. 1.0) in
      * let invert_t = 1.0 -. t in
      * Vec3.plus (Vec3.mulf (Vec3.create 1.0 1.0 1.0) invert_t)
      *           (Vec3.mulf (Vec3.create 0.3 0.2 0.5) t) *)

let random_mat () =
  let r = Random.float 1.0 in
  if Caml.(<)  r 0.6 then
    Material.Lambertian (Vec3.create
                           ((Random.float 1.0) *. (Random.float 1.0))
                           ((Random.float 1.0) *. (Random.float 1.0))
                           ((Random.float 1.0) *. (Random.float 1.0)))
  else
    if Caml.(<) r 0.8 then
      Material.Metal ((Vec3.create
                        (0.5 *. (1.0 +. (Random.float 1.0)))
                        (0.5 *. (1.0 +. (Random.float 1.0)))
                        (0.5 *. (1.0 +. (Random.float 1.0)))), 0.5 *. (Random.float 1.0))
    else
      Material.Emission ((Vec3.create
                        (0.5 *. (1.0 +. (Random.float 1.0)))
                        (0.5 *. (1.0 +. (Random.float 1.0)))
                        (0.5 *. (1.0 +. (Random.float 1.0)))), 4.0 *. (Random.float 1.0))

let ab_list =
  List.concat (List.map (List.range (-11) 11) ~f:(fun a ->
                          List.map (List.range (-11) 11) ~f:(fun b ->
                                     (a, b))))

let balls =
  List.map ab_list ~f:(fun (a, b) ->
                          let mat = random_mat () in
                          let center = Vec3.create
                                         ((Int.to_float a) +. 0.9 *. (Random.float 1.0))
                                         0.2
                                         ((Int.to_float b) +. 0.9 *. (Random.float 1.0)) in
                          (Hitable.sphere center 0.2 mat))
  
let scene = Hitable.of_list [
                (* Hitable.sphere
                 *   (Vec3.create 0.0 0.0 0.0) 1.0 (Material.Metal ((Vec3.create 0.3 0.4 0.9), 1.0)); *)
                (* 手前壁 *)
                Hitable.square (Vec3.create 1.0 2.0 (-2.0)) (Vec3.create (-1.0) 2.0 (-2.0)) (Vec3.create (-1.0) 0.0 (-2.0)) (Vec3.create 1.0 0.0 (-2.0))
                               (Material.Lambertian (Vec3.create 0.8 0.8 0.8));
                (* 正面壁 *)
                Hitable.square (Vec3.create 1.0 2.0 0.0) (Vec3.create 1.0 0.0 0.0) (Vec3.create (-1.0) 0.0 0.0) (Vec3.create (-1.0) 2.0 0.0)
                               (Material.Lambertian (Vec3.create 0.8 0.8 0.8));
                (* 天井 *)
                Hitable.square (Vec3.create 1.0 2.0 0.0) (Vec3.create (-1.0) 2.0 0.0) (Vec3.create (-1.0) 2.0 (-2.0)) (Vec3.create 1.0 2.0 (-2.0))
                               (Material.Lambertian (Vec3.create 0.8 0.8 0.8));
                (* 左壁 *)
                Hitable.square (Vec3.create 1.0 2.0 0.0) (Vec3.create 1.0 2.0 (-2.0)) (Vec3.create 1.0 0.0 (-2.0)) (Vec3.create 1.0 0.0 0.0)
                               (Material.Lambertian (Vec3.create 0.8 0.1 0.1));
                (* 右壁 *)
                Hitable.square (Vec3.create (-1.0) 2.0 0.0) (Vec3.create (-1.0) 0.0 0.0) (Vec3.create (-1.0) 0.0 (-2.0)) (Vec3.create (-1.0) 2.0 (-2.0))
                               (Material.Lambertian (Vec3.create 0.1 0.1 0.8));
                (* 床壁 *)
                Hitable.square (Vec3.create 1.0 0.0 0.0) (Vec3.create 1.0 0.0 (-2.0)) (Vec3.create (-1.0) 0.0 (-2.0)) (Vec3.create (-1.0) 0.0 0.0) 
                               (Material.Lambertian (Vec3.create 0.9 0.9 0.9));
                (* ライト *)
                Hitable.sphere (Vec3.create 0.0 2.2 (-1.0)) 0.3 (Material.Emission ((Vec3.create 1.0 1.0 1.0), 20.0));
                (* 左たま *)
                Hitable.sphere (Vec3.create 0.5 0.4 (-0.8)) 0.4 (Material.Metal ((Vec3.create 1.0 1.0 1.0), 0.0));
                (* 右たま *)
                Hitable.sphere (Vec3.create (-0.5) 0.3 (-1.4)) 0.3 (Material.Dielectric 1.5);
                (* Hitable.of_list balls; *)
]

(* サンプリング1回 *)
let sample u v =
  let r = Camera.get_ray (Camera.create nx ny) u v in
  color r scene 1

(* サンプリングns回 *)
let sample_ns x y =
  let col = List.fold (List.range 0 ns) ~init:(Vec3.create 0.0 0.0 0.0)
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
   

