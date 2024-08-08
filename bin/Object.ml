open Shape
open Material
open HitRecord
open AABB
open Transform

module Object = struct
    type object_T = {
        shape: Shape.shape_T;
        material: Material.material_T;
        mutable aabb : AABB.aabb_T;
        id: int;
        mutable transforms: Transform.transform_T array;
    }

    let generate_id =
        let count = ref 0 in
        fun () ->
            let id = !count in
            incr count;
            id

    let create s m =
        let aabb = Shape.create_bounding_box s in
        {
            shape = s;
            material = m;
            aabb = aabb;
            id = generate_id ();
            transforms = [||]
        }

    let add_transform o t =
        o.aabb <- Transform.apply_aabb t o.aabb;
        o.transforms <- Array.append o.transforms [|t|]

    let get_bounding_box o =
        o.aabb

    let check_collision o r interval =
        let new_ray = Array.fold_left (fun ray t -> Transform.apply t ray) r o.transforms in
        let hit = Shape.check_collision new_ray o.shape interval in
        match hit with
        | HitRecord.Miss -> HitRecord.Miss
        | HitRecord.Hit h ->
            h.id <- o.id;
            let new_hit_record = Array.fold_right (fun t hit -> Transform.unapply t hit) o.transforms h in
            HitRecord.Hit new_hit_record

    let scatter_ray o r h =
        (* let colour = Vec3.scalar (Vec3.add h.normal Vec3.one) 0.5 in *)
        (* Some (colour, r) *)
        Material.scatter o.material r h

    let to_string o =
        Printf.sprintf "(%s | %d)" (Shape.string_of_shape o.shape) o.id
end
