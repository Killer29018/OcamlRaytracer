open Vec3

module HitRecord = struct
    type hit_record = {
        mutable t: float;
        mutable pos: Vec3.vec3;
        mutable normal: Vec3.vec3;
        mutable is_front_face: bool;
    }

    type hit = Miss
             | Hit of hit_record

    let create_null_hit_record =
        fun () -> {
                    t = 0.;
                    pos = Vec3.zero;
                    normal = Vec3.zero;
                    is_front_face = false
                  }

    let create_hit_record_tp t p =
        let record = create_null_hit_record () in
        record.t <- t; record.pos <- p; record

    let create_hit_record_tpn t p n =
        let record = create_null_hit_record () in
        record.t <- t;
        record.pos <- p;
        record.normal <- n;
        record

    let create_hit_record_tpnf t p n f =
        let record = create_null_hit_record () in
        record.t <- t;
        record.pos <- p;
        record.normal <- n;
        record.is_front_face <- f;
        record

    let string_of_hit_record h =
        Printf.sprintf
            "%.4f | (%s) | (%s) | %s"
            (h.t)
            (Vec3.string_of_vec3 h.pos)
            (Vec3.string_of_vec3 h.normal)
            (if h.is_front_face then "FRONT" else "BACK")

    let string_of_hit = function
        | Miss -> Printf.sprintf "MISS"
        | Hit h -> Printf.sprintf "HIT | %s" (string_of_hit_record h)
end
