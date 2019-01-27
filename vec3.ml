open Base

module Vec3 = struct
  type t = {
      x:float;
      y:float;
      z:float;
    }

  let create x y z = {x=x;y=y;z=z}

  let xyz v = (v.x, v.y, v.z)

  let squared_length v = v.x *. v.x +. v.y *. v.y +. v.z *. v.z
  let length v = sqrt @@ squared_length v
end

