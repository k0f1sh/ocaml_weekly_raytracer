(* open Base *)

let test_set = [
    "vec3" , `Quick, Vec3Test.test;
  ]
             
let () =
  Alcotest.run "My test" [
                 "test_set", test_set;
               ]

