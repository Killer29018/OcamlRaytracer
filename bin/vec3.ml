module Vec3 = struct
    type vec3 = {
        x: float;
        y: float;
        z: float
    }

    let string_of_vec3 x =
        let linearToGamma a =
            if a > 0. then
                Float.sqrt a
            else
                0.
        in
        let r = int_of_float ((linearToGamma x.x) *. 255.) in
        let g = int_of_float ((linearToGamma x.y) *. 255.) in
        let b = int_of_float ((linearToGamma x.z) *. 255.) in
        Printf.sprintf "%d %d %d" r g b

    let newVec3 x y z = { x = x; y = y; z = z }

    let vec3Zero = newVec3 0. 0. 0.

    let scalar r s = { x = r.x *. s; y = r.y *. s; z = r.z *. s }

    let add a b =
        { x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z }

    let sub a b =
        { x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z }

    let addM xs =
        List.fold_left (fun x y -> add x y) vec3Zero xs

    let subM xs =
        List.fold_left (fun x y -> sub x y) vec3Zero xs

    let compMul a b =
        { x = a.x *. b.x; y = a.y *. b.y; z = a.z *. b.z }

    let dot a b = a.x *. b.x +. a.y *. b.y +. a.z *. b.z

    let lerp a b t =
        let x = a.x *. (1. -. t) +. b.x *. t in
        let y = a.y *. (1. -. t) +. b.y *. t in
        let z = a.z *. (1. -. t) +. b.z *. t in
        { x = x; y = y; z = z }

    let negate a =
        { x = ~-.(a.x); y = ~-.(a.y); z = ~-.(a.z); }

    let mag x =
        sqrt (dot x x)

    let mag_squared x = dot x x

    let norm x =
        let factor = 1. /. (mag x) in
        { x = factor *. x.x; y = factor *. x.y; z = factor *. x.z }

    let compMax a b =
        { x = max a.x b.x; y = max a.y b.y; z = max a.z b.z }

    let nearZero x =
        let s = 1e-8 in
        Float.abs x.x < s && Float.abs x.y < s && Float.abs x.z < s

    let randomVec3 =
        fun () ->
            let x = Random.float 1. in
            let y = Random.float 1. in
            let z = Random.float 1. in
            { x = x; y = y; z = z }

    let randomUnitVec3 =
        fun () ->
            let x = Random.float 1. in
            let y = Random.float 1. in
            let z = Random.float 1. in
            norm { x = x; y = y; z = z }

    let randomVec3Bounds min max =
        let x = min +. (Random.float (max -. min)) in
        let y = min +. (Random.float (max -. min)) in
        let z = min +. (Random.float (max -. min)) in
        { x = x; y = y; z = z }

    let randomUnitSphere =
        fun () ->
            let quitLoop = ref false in
            let ray = ref vec3Zero in
            while not !quitLoop; do
                ray := (randomVec3Bounds ~-.1. 1.);
                if (mag_squared !ray) < 1. then
                    quitLoop := true
            done;
        !ray

    let randomHemisphere normal =
        let unitSphere = randomUnitSphere () in
        if (dot unitSphere normal) > 0. then
            unitSphere
        else
            negate unitSphere
end
