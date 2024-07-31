module Vec3 = struct
    type vec3 = {
        x: float;
        y: float;
        z: float
    }

    let create x y z =
        { x = x; y = y; z = z }

    let create_single t =
        { x = t; y = t; z = t }

    let zero = create 0. 0. 0.
    let one = create 1. 1. 1.

    let p_x = create 1. 0. 0.
    let p_y = create 0. 1. 0.
    let p_z = create 0. 0. 1.

    let n_x = create ~-.1. 0. 0.
    let n_y = create 0. ~-.1. 0.
    let n_z = create 0. 0. ~-.1.

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

    let random_bounds min max =
        let x = min +. (Random.float (max -. min)) in
        let y = min +. (Random.float (max -. min)) in
        let z = min +. (Random.float (max -. min)) in
        create x y z

    let random_in_unit_disk =
        fun () ->
            let v = ref (random_bounds ~-.1. 1.) in
            v := (create !v.x !v.y 0.);
            while not ((mag_squared !v) < 1.) do
                let new_v = random_bounds ~-.1. 1. in
                let new_v = create new_v.x new_v.y 0. in
                v := new_v;
            done; !v

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

    let reflect v n =
        sub v (scalar n (2. *. (dot v n)))

    let refract uv n e =
        let cos_theta = min (dot (negate uv) n) 1. in
        let r_out_perp = scalar (add uv (scalar n cos_theta)) e in
        let r_out_para = scalar n (~-.(sqrt (Float.abs (1. -. (mag_squared r_out_perp))))) in
        add r_out_perp r_out_para

    let lerp a b t =
        add (scalar a (1. -. t)) (scalar b t)

    let min_comp a b = {
            x = min a.x b.x;
            y = min a.y b.y;
            z = min a.z b.z;
        }

    let max_comp a b = {
            x = max a.x b.x;
            y = max a.y b.y;
            z = max a.z b.z;
        }
end
