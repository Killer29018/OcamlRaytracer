open Shape
open Ray
open HitRecord
(* open Vec3 *)
open Material
module Object = struct
    type object_T = {
        shape: Shape.shape_T;
        material: Material.material_T;
    }

    let create s m = {
        shape = s; material = m
    }

    let scatter_ray o (r : Ray.ray) (h : HitRecord.hit_record) =
        Material.scatter_ray o.material r h

    let check_collision o r =
        Shape.check_collision r o.shape
end
