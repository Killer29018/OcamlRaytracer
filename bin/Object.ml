open Shape
open Material
open HitRecord

module Object = struct
    type object_T = {
        shape: Shape.shape_T;
        material: Material.material_T;

        id: int;
    }

    let generate_id =
        let count = ref 0 in
        fun () ->
            let id = !count in
            incr count;
            id

    let create s m =
        { shape = s; material = m; id = generate_id ()}

    let get_bounding_box o =
        Shape.get_bounding_box o.shape

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
end
