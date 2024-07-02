open Object

module Scene : sig
    type scene_definition = {
        mutable name: string;
        mutable objects: Object.object_T array; (* Objects *)

        mutable image_width: int;
        mutable image_height: int;

        mutable viewport_width: float;
        mutable viewport_height: float;
        mutable viewport_depth: float;

        mutable max_depth: int;
    }

    val create_null_definition : (unit -> scene_definition)
    val create_null_definition_with_objects : Object.object_T list -> scene_definition

    val add_object : scene_definition -> Object.object_T -> scene_definition

    val render_scene : scene_definition -> unit
end
