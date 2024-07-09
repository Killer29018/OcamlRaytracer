module Vec3 = struct
    type vec3 = {
        x: float;
        y: float;
        z: float;
    }

    let create x y z =
        { x = x; y = y; z = z }

    let zero = create 0. 0. 0.
    let one = create 1. 1. 1.

    let add a b =
        { x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z }

    let sub a b =
        { x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z }

    let dot a b =
        a.x *. b.x +. a.y *. b.y +. a.z *. b.z

    let scalar a s =
        { x = a.x *. s; y = a.y *. s; z = a.z *. s }

    let comp_mul a b =
        { x = a.x *. b.x; y = a.y *. b.y; z = a.z *. b.z }

    let mag_squared v =
        dot v v

    let mag v =
        sqrt (mag_squared v)

    let norm v =
        scalar v (1. /. (mag v))

    let negate v =
        scalar v ~-.1.

    let random_bounds min max =
        let x = min +. (Random.float (max -. min)) in
        let y = min +. (Random.float (max -. min)) in
        let z = min +. (Random.float (max -. min)) in
        create x y z

    let random_in_unit_sphere =
        fun () ->
            let v = ref (random_bounds ~-.1. 1.) in
            while not ((mag_squared !v) < 1.) do
                let new_v = random_bounds ~-.1. 1. in
                v := new_v;
            done; !v

    let random_unit =
        fun () ->
            norm (random_in_unit_sphere ())

    let near_zero v =
        let eps = 1e-8 in
        (mag_squared v) < eps
end
