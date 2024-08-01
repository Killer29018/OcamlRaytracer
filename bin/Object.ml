open Shape
open Material
open HitRecord
open AABB

module Object = struct
    type object_T = {
        shape: Shape.shape_T;
        material: Material.material_T;
        aabb : AABB.aabb_T;
        id: int;
    }

    let generate_id =
        let count = ref 0 in
        fun () ->
            let id = !count in
            incr count;
            id

    let create s m =
        let aabb = Shape.create_bounding_box s in
        { shape = s; material = m; aabb = aabb; id = generate_id ()}

    let get_bounding_box o =
        o.aabb

    let check_collision o r interval =
        let hit = Shape.check_collision r o.shape interval in
        match hit with
        | HitRecord.Miss -> HitRecord.Miss
        | HitRecord.Hit h ->
            h.id <- o.id;
            HitRecord.Hit h

    let scatter_ray o r h =
        (* let colour = Vec3.scalar (Vec3.add h.normal Vec3.one) 0.5 in *)
        (* Some (colour, r) *)
        Material.scatter o.material r h

    let to_string o =
        Printf.sprintf "(%s | %d)" (Shape.string_of_shape o.shape) o.id
end
