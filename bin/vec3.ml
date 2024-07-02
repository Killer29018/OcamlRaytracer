module Vec3 = struct
    type vec3 = {
        x: float;
        y: float;
        z: float
    }

    let newV x y z =
        { x = x; y = y; z = z }

    let zero = newV 0. 0. 0.
    let one = newV 1. 1. 1.

    let string_of_vec3 x =
        Printf.sprintf "%f %f %f" x.x x.y x.z

    let add a b =
        { x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z }

    let add_list xs =
        List.fold_left (add) zero xs

    let sub a b =
        { x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z }

    let sub_list xs =
        List.fold_left (sub) zero xs

    let dot a b =
        a.x *. b.x +. a.y *. b.y +. a.z *. b.z

    let cross a b =
        { x = a.y *. b.z -. a.z *. b.y;
          y = a.z *. b.x -. a.x *. b.z;
          z = a.x *. b.y -. a.y *. b.x }

    let scalar a s =
        { x = a.x *. s; y = a.y *. s; z = a.z *. s }

    let comp_mul a b =
        { x = a.x *. b.x; y = a.y *. b.y; z = a.z *. b.z }

    let comp_mul_list xs =
        List.fold_left (comp_mul) one xs

    let mag_squared v =
        dot v v

    let mag v =
        sqrt (mag_squared v)

    let norm v =
        scalar v (1. /. (mag v))

    let negate v =
        scalar v ~-.1.
end
