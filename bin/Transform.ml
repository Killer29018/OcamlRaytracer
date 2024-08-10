open Vec3
open HitRecord
open AABB
open Ray
open Misc

module Transform = struct
    type translate_data = {
        offset: Vec3.vec3;
    }

    type rotate_y_data = {
        sin_theta: float;
        cos_theta: float;
    }

    type transform_T = None
                     | Translate of translate_data
                     | RotateY of rotate_y_data

    let create_null = None

    let create_translate p =
        Translate { offset = p }

    let create_rotate_y angle =
        let radians = radians_of_deg angle in
        RotateY { sin_theta = sin radians; cos_theta = cos radians }

    let apply transform (ray : Ray.ray) =
        match transform with
        | None -> ray
        | RotateY r ->
                let new_origin = Vec3.create
                    (r.cos_theta *. ray.origin.x -. r.sin_theta *. ray.origin.z)
                    ray.origin.y
                    (r.sin_theta *. ray.origin.x +. r.cos_theta *. ray.origin.z)
                in
                let new_direction = Vec3.create
                    (r.cos_theta *. ray.direction.x -. r.sin_theta *. ray.direction.z)
                    ray.direction.y
                    (r.sin_theta *. ray.direction.x +. r.cos_theta *. ray.direction.z)
                in
                Ray.create new_origin new_direction
        | Translate t -> Ray.create (Vec3.sub ray.origin t.offset) ray.direction

    let apply_aabb transform (aabb : AABB.aabb_T) =
        match transform with
        | None -> aabb
        | RotateY r ->
                let min = ref (Vec3.create Float.infinity Float.infinity Float.infinity) in
                let max = ref (Vec3.create Float.neg_infinity Float.neg_infinity Float.neg_infinity) in

                let i = ref 0 in
                let j = ref 0 in
                let k = ref 0 in
                while !i != 2 do
                    let fi = float_of_int !i in
                    let fj = float_of_int !j in
                    let fk = float_of_int !k in
                    let x = fi *. aabb.x.max +. (1. -. fi) *. aabb.x.min in
                    let y = fj *. aabb.y.max +. (1. -. fj) *. aabb.y.min in
                    let z = fk *. aabb.z.max +. (1. -. fk) *. aabb.z.min in

                    let new_x = r.cos_theta *. x +. r.sin_theta *. z in
                    let new_z = ~-.(r.sin_theta) *. x +. r.cos_theta *. z in

                    min := Vec3.create
                        (Float.min (!min).x new_x)
                        (Float.min (!min).y y)
                        (Float.min (!min).z new_z);
                    max := Vec3.create
                        (Float.max (!max).x new_x)
                        (Float.max (!max).y y)
                        (Float.max (!max).z new_z);

                    k := !k + 1;
                    if !k = 2 then
                        k := 0;
                        j := !j + 1;
                        if !j = 2 then
                            j := 0;
                            i := !i + 1
                done;
                AABB.create_points !min !max
        | Translate t -> AABB.offset aabb t.offset

    let unapply transform (hit : HitRecord.hit_record) =
        match transform with
        | None -> hit
        | RotateY r ->
            let new_pos = Vec3.create
                (r.cos_theta *. hit.pos.x +. r.sin_theta *. hit.pos.z)
                hit.pos.y
                (~-.(r.sin_theta) *. hit.pos.x +. r.cos_theta *. hit.pos.z)
            in
            let new_normal = Vec3.create
                (r.cos_theta *. hit.normal.x +. r.sin_theta *. hit.normal.z)
                hit.normal.y
                (~-.(r.sin_theta) *. hit.normal.x +. r.cos_theta *. hit.normal.z)
            in
            hit.pos <- new_pos;
            hit.normal <- new_normal;
            hit
        | Translate t ->
            hit.pos <- Vec3.add hit.pos t.offset;
            hit
end
