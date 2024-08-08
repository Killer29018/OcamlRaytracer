open Vec3
open HitRecord
open AABB
open Ray

module Transform = struct
    type translate_data = {
        offset: Vec3.vec3;
    }

    type rotate_y_data = {
        rotation: float;
    }

    type transform_T = None
                     | Translate of translate_data
                     | RotateY of rotate_y_data

    let create_null = None

    let create_translate p =
        Translate { offset = p }

    let apply transform (ray : Ray.ray) =
        match transform with
        | None -> ray
        | Translate t -> Ray.create (Vec3.sub ray.origin t.offset) ray.direction

    let apply_aabb transform (aabb : AABB.aabb_T) =
        match transform with
        | None -> aabb
        | Translate t -> AABB.offset aabb t.offset

    let unapply transform (hit : HitRecord.hit_record) =
        match transform with
        | None -> hit
        | Translate t ->
            hit.pos <- Vec3.add hit.pos t.offset;
            hit
end
