open Base
module Out = Stdio.Out_channel

let nx = 200
let ny = 100
let output_file_name = "output.ppm"

(* 1ピクセル分を描画する *)
(* Out_channel.t -> int -> int -> unit *)
let render chan x y =
  let r = (Int.to_float x) /. (Int.to_float nx) in
  let g = (Int.to_float y) /. (Int.to_float ny) in
  let b = 0.2 in
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
   


