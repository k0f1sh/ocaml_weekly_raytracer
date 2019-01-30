open Base

type t = {tf: float;
          p: Vec3.t;
          normal: Vec3.t;}

let create tf p normal = { tf; p; normal }
let tf hit_record = hit_record.tf
let p hit_record = hit_record.p
let normal hit_record = hit_record.normal

