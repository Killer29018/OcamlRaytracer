module IntersectionCount = struct
    type count = {
        mutable total : int;
        mutable sphere : int;
        mutable box : int;
        mutable quad : int;
    }

    let global_count = ref {total = 0; sphere = 0; box = 0; quad = 0}

    let increment_sphere =
        fun () ->
            let count = !global_count in
            count.total <- count.total + 1;
            count.sphere <- count.sphere + 1;
            global_count := count

    let increment_box =
        fun () ->
            let count = !global_count in
            count.total <- count.total + 1;
            count.box <- count.box + 1;
            global_count := count

    let increment_quad =
        fun () ->
            let count = !global_count in
            count.total <- count.total + 1;
            count.quad <- count.quad + 1;
            global_count := count

    let get_total =
        fun () ->
            let count = !global_count in
            count.total

    let get_sphere =
        fun () ->
            let count = !global_count in
            count.sphere

    let get_box =
        fun () ->
            let count = !global_count in
            count.box

    let get_quad =
        fun () ->
            let count = !global_count in
            count.quad

    let print_intersections =
        fun () ->
            let total = get_total () in
            let sphere = get_sphere () in
            let box = get_box () in
            let quad = get_quad () in

            Printf.printf "INTERSECTIONS\n";
            Printf.printf "Total: %d\n" total;
            Printf.printf "\tSphere: %d\n" sphere;
            Printf.printf "\tBox: %d\n" box;
            Printf.printf "\tQuad: %d\n" quad;
end
