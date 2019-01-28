(* open Base *)
module Vec3 = Raylib.Vec3

let test () =
  (* FIXME Alcotest.floatがよくわからない *)
  Alcotest.(check (Alcotest.float 0.0)) "vec x" 1.0 @@ Vec3.x (Vec3.create 1.0 2.0 3.0);
  Alcotest.(check (Alcotest.float 0.0)) "vec length" 3.7416573867739413 @@ Vec3.length (Vec3.create 1.0 2.0 3.0);
  Alcotest.(check (Alcotest.float 0.0)) "vec length" 14.0 @@ Vec3.squared_length (Vec3.create 1.0 2.0 3.0)
