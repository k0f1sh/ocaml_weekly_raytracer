open Base
type t = {
    x:float;
    y:float;
    z:float;
  }

let create x y z = { x; y; z }

let xyz v = (v.x, v.y, v.z)

let x v = v.x
let y v = v.y
let z v = v.z

let squared_length v = v.x *. v.x +. v.y *. v.y +. v.z *. v.z

let length v = Float.sqrt @@ squared_length v

let make_unit_vector v =
  let k = 1.0 /. (Float.sqrt (v.x *. v.x +. v.y *. v.y +. v.z *. v.z))
  in create (v.x *. k) (v.y *. k) (v.z *. k)

let plus v1 v2 = { x = v1.x +. v2.x;
                   y = v1.y +. v2.y;
                   z = v1.z +. v2.z}

let minus v1 v2 = { x = v1.x -. v2.x;
                    y = v1.y -. v2.y;
                    z = v1.z -. v2.z}

let mul v1 v2 = { x = v1.x *. v2.x;
                  y = v1.y *. v2.y;
                  z = v1.z *. v2.z}

let div v1 v2 = { x = v1.x /. v2.x;
                  y = v1.y /. v2.y;
                  z = v1.z /. v2.z}

let dot v1 v2 = v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z

let cross v1 v2 = {x = (v1.y *. v2.z) -. (v1.z *. v2.y);
                   y = (-1.0) *. ((v1.x *. v2.z) -. (v1.z *. v2.x));
                   z = (v1.x *. v2.y) -. (v1.y *. v2.x)}

(* *. より後に定義するとfloat -> float の *.を指さなくなる *)
let divf v f = let k = 1.0 /. f in
               { x = v.x *. k;
                 y = v.y *. k;
                 z = v.z *. k}

let mulf v f = { x = v.x *. f;
                 y = v.y *. f;
                 z = v.z *. f}

let unit_vector v = divf v (length v)

let reflect v n = minus v (mulf n ((dot v n) *. 2.0))

let refract v n ni_over_nt =
  let uv = unit_vector v in
  let dt = dot uv n in
  let discriminant = 1.0 -. ni_over_nt *. ni_over_nt *. (1.0 -. dt *. dt) in
  if Caml.(>) discriminant 0.0 then
    Some (minus
            (mulf (minus uv (mulf n dt)) ni_over_nt)
            (mulf n (Float.sqrt discriminant)))
  else
    None

(* (defn get-normal [v0 v1 v2]
 *   (let [t0 (v/sub v1 v0)
 *         t1 (v/sub v2 v0)]
 *     (v/unit-vector (v/cross t1 t0)))) *)

let normal v1 v2 v3 =
  let t0 = minus v2 v1 in
  let t1 = minus v3 v1 in
  unit_vector @@ cross t1 t0
