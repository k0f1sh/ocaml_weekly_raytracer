open Base
module Out = Stdio.Out_channel
module Vec3 = Raylib.Vec3

let nx = 200
let ny = 100
let output_file_name = "output.ppm"

(* 1ピクセル分を描画する *)
(* Out_channel.t -> int -> int -> unit *)
let render chan x y =
  let x = (Int.to_float x) /. (Int.to_float nx) in
  let y = (Int.to_float y) /. (Int.to_float ny) in
  let z = 0.2 in
  let v = Vec3.create x y z in
  let (r, g, b) = Vec3.xyz v in
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
   


