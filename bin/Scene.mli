open Object
open Camera
open Viewport

module Scene : sig
    type scene_definition = {
        mutable name: string;
        mutable objects: Object.object_T array; (* Objects *)

        mutable image_width: int;
        mutable image_height: int;

        mutable viewport: Viewport.viewport_T;

        mutable camera: Camera.camera_T;

        mutable max_depth: int;
        mutable sample_count: int;
    }

    val create_null_definition : (unit -> scene_definition)
    val create_null_definition_with_objects : Object.object_T list -> scene_definition

    val add_object : scene_definition -> Object.object_T -> scene_definition

    val render_scene : scene_definition -> unit
end
