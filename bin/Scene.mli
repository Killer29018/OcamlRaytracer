open Camera
open Viewport
open BVH_Node
open Object

module Scene : sig
    type scene_definition = {
        mutable name: string;
        mutable bvh: BVH_Node.bvh_node;
        objects: Object.object_T array;

        mutable image_width: int;
        mutable image_height: int;

        mutable viewport: Viewport.viewport_T;

        mutable camera: Camera.camera_T;

        mutable max_depth: int;
        mutable sample_count: int;
    }

    val create_null_definition : (unit -> scene_definition)
    val create_null_definition_with_bvh : BVH_Node.bvh_node -> Object.object_T array -> scene_definition

    val render_scene : scene_definition -> unit
end
