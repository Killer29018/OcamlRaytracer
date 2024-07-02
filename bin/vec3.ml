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

    let comp_abs x =
        { x = Float.abs x.x; y = Float.abs x.y; z = Float.abs x.z }

    let mag_squared v =
        dot v v

    let mag v =
        sqrt (mag_squared v)

    let norm v =
        scalar v (1. /. (mag v))

    let negate v =
        scalar v ~-.1.

    let random_unit =
        fun () ->
            let x = Random.float 1. in
            let y = Random.float 1. in
            let z = Random.float 1. in
            norm (newV x y z)

    let random_bounds min max =
        let x = min +. (Random.float (max -. min)) in
        let y = min +. (Random.float (max -. min)) in
        let z = min +. (Random.float (max -. min)) in
        newV x y z

    let near_zero v =
        let eps = 1e-8 in
        (mag_squared v) < eps
end
