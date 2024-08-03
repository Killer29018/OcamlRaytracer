open Vec3
open Vec2

module HitRecord = struct
    type hit_record = {
        mutable t: float;
        mutable pos: Vec3.vec3;
        mutable normal: Vec3.vec3;
        mutable is_front_face: bool;
        mutable uv : Vec2.vec2;
        mutable id: int;
    }

    type hit = Miss
             | Hit of hit_record

    let create_null_hit_record =
        fun () -> {
                    t = 0.;
                    pos = Vec3.zero;
                    normal = Vec3.zero;
                    is_front_face = false;
                    uv = Vec2.zero;
                    id = -1;
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

    let closest_hit a b =
        match (a, b) with
        | Miss, Miss -> Miss
        | Miss, _ -> b
        | _, Miss -> a
        | Hit h1, Hit h2 ->
            if h1.t < h2.t then Hit h1 else Hit h2

    let string_of_hit_record h =
        Printf.sprintf
            "%.4f | (%s) | (%s) | %s | (%s) | %d"
            (h.t)
            (Vec3.string_of_vec3 h.pos)
            (Vec3.string_of_vec3 h.normal)
            (if h.is_front_face then "FRONT" else "BACK")
            (Vec2.string_of_vec2 h.uv)
            (h.id)

    let string_of_hit = function
        | Miss -> Printf.sprintf "MISS"
        | Hit h -> Printf.sprintf "HIT | %s" (string_of_hit_record h)
end
