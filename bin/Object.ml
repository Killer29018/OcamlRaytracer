open Shape
open HitRecord
open Vec3
open Ray

module Object = struct
    type object_T = {
        shape: Shape.shape_T
    }

    let create s =
        { shape = s }

    let check_collision o r =
        Shape.check_collision r o.shape

    let scatter_ray _o (_r : Ray.ray) (h : HitRecord.hit_record) =
        let colour = Vec3.comp_abs (Vec3.norm h.normal) in
        let new_direction = Vec3.norm h.normal in
        let new_origin = Vec3.add h.pos (Vec3.scalar new_direction 0.001) in
        let new_ray = Ray.create new_origin new_direction in
        Some (colour, new_ray)
end
