open Shape
open Material

module Object = struct
    type object_T = {
        shape: Shape.shape_T;
        material: Material.material_T;
    }

    let create s m =
        { shape = s; material = m }

    let check_collision o r =
        Shape.check_collision r o.shape

    let scatter_ray o r h =
        Material.scatter o.material r h
end
