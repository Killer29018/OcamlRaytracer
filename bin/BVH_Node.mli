open AABB
open Object
open HitRecord
open Interval
open Ray

module BVH_Node : sig
    type bvh_node = {
        aabb: AABB.aabb_T;
        left: node;
        right: node
    }
    and node = Object of Object.object_T
             | Node of bvh_node
             | None

    val create_null : (unit -> bvh_node)

    val create_span : Object.object_T array -> int -> int -> bvh_node

    val create : Object.object_T array -> bvh_node

    val check_collision : Ray.ray -> bvh_node -> Interval.interval_T -> HitRecord.hit

    val to_string : bvh_node -> string
end
