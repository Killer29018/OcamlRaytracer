open Vec3

module Perlin = struct
    type perlin_T = {
        mutable perm_x : int array;
        mutable perm_y : int array;
        mutable perm_z : int array;
        mutable rand_float : float array;
    }

    let create_null =
        fun () -> {
            perm_x = [||];
            perm_y = [||];
            perm_z = [||];
            rand_float = [||];
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
            perlin.rand_float <- Array.init point_count (fun _ -> Random.float 1.);
            perlin.perm_x <- Array.init point_count (fun i -> i);
            perlin.perm_y <- Array.init point_count (fun i -> i);
            perlin.perm_z <- Array.init point_count (fun i -> i);

            perlin.perm_x <- (permute perlin.perm_x point_count);
            perlin.perm_y <- (permute perlin.perm_y point_count);
            perlin.perm_z <- (permute perlin.perm_z point_count);
            perlin

    let noise perlin (p : Vec3.vec3) =
        let i = Int.logand (int_of_float (4. *. p.x)) 255 in
        let j = Int.logand (int_of_float (4. *. p.y)) 255 in
        let k = Int.logand (int_of_float (4. *. p.z)) 255 in

        let i1 = Array.get perlin.perm_x i in
        let i2 = Array.get perlin.perm_y j in
        let i3 = Array.get perlin.perm_z k in

        Array.get (perlin.rand_float) (Int.logxor i1 (Int.logxor i2  i3))
end
