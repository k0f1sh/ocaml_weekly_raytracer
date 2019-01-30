open Base

type t = {mutable tf: float;
          mutable p: Vec3.t;
          mutable normal: Vec3.t;}

let update hit_record tf p normal = begin
    hit_record.tf <- tf;
    hit_record.p <- p;
    hit_record.normal <- normal;
    ()
  end

let tf hit_record = hit_record.tf
let p hit_record = hit_record.p

