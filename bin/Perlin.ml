open Vec3

module Perlin = struct
    type perlin_T = {
        mutable perm_x : int array;
        mutable perm_y : int array;
        mutable perm_z : int array;
        mutable rand_vec : Vec3.vec3 array;
    }

    let create_null =
        fun () -> {
            perm_x = [||];
            perm_y = [||];
            perm_z = [||];
            rand_vec = [||];
        }

    let permute a n =
        let i = ref (n - 1) in
        while (!i > 0) do
            let target = Random.int !i in
            let tmp = a.(!i) in
            a.(!i) <- (Array.get a target);
            a.(target) <- tmp;
            i := !i - 1
        done; a

    let create =
        fun () ->
            let perlin = create_null () in
            let point_count = 256 in
            perlin.rand_vec <- Array.init point_count (fun _ -> Vec3.random_bounds ~-.1. 1.);
            perlin.perm_x <- Array.init point_count (fun i -> i);
            perlin.perm_y <- Array.init point_count (fun i -> i);
            perlin.perm_z <- Array.init point_count (fun i -> i);

            perlin.perm_x <- (permute perlin.perm_x point_count);
            perlin.perm_y <- (permute perlin.perm_y point_count);
            perlin.perm_z <- (permute perlin.perm_z point_count);
            perlin

    let perlin_interp c u v w =
        let uu = u *. u *. (3. -. 2. *. u) in
        let vv = v *. v *. (3. -. 2. *. v) in
        let ww = w *. w *. (3. -. 2. *. w) in
        let accum = ref 0. in

        let _ = Array.mapi (fun i x ->
                    Array.mapi (fun j y ->
                        Array.mapi (fun k c ->
                            let i = float_of_int i in
                            let j = float_of_int j in
                            let k = float_of_int k in
                            let weight_v = Vec3.create (u -. i) (v -. j) (w -. k) in

                            accum := (!accum) +. (i *. uu +. (1. -. i) *. (1. -. u)) *.
                                                 (j *. vv +. (1. -. j) *. (1. -. v)) *.
                                                 (k *. ww +. (1. -. k) *. (1. -. w)) *. (Vec3.dot c weight_v)
                ) y) x) c in
        !accum

    let noise perlin (p : Vec3.vec3) =
        let u = p.x -. (Float.floor p.x) in
        let v = p.y -. (Float.floor p.y) in
        let w = p.z -. (Float.floor p.z) in

        let i = int_of_float (Float.floor p.x) in
        let j = int_of_float (Float.floor p.y) in
        let k = int_of_float (Float.floor p.z) in

        let c = Array.init 2
            (fun _ -> Array.init 2 (fun _ ->
                Array.make 2 Vec3.zero)) in

        let c = Array.mapi (fun di x ->
                    Array.mapi (fun dj y ->
                        Array.mapi (fun dk _ ->
                            let index = perlin.perm_x.(Int.logand (i + di) 255) in
                            let index = Int.logxor (perlin.perm_y.(Int.logand (j + dj) 255)) index in
                            let index = Int.logxor (perlin.perm_z.(Int.logand (k + dk) 255)) index in
                            perlin.rand_vec.(index)
                ) y) x) c in
        perlin_interp c u v w

    let turb perlin (p : Vec3.vec3) depth =
        let accum = ref 0. in
        let temp_p = ref p in
        let weight = ref 1. in

        let i = ref 0 in
        while !i < depth do
            accum := !accum +. !weight *. (noise perlin p);
            weight := !weight *. 0.5;
            temp_p := Vec3.scalar !temp_p 2.;
            i := !i + 1
        done; (Float.abs !accum)
end
