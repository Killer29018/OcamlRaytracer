open Shape
open Material
open Ray
open HitRecord
open Vec3
open Interval
open AABB
open Transform

module Object : sig
    type object_T = {
        shape: Shape.shape_T;
        material: Material.material_T;
        mutable aabb : AABB.aabb_T;
        id: int;
        mutable transforms: Transform.transform_T array;
    }

    val create : Shape.shape_T -> Material.material_T -> object_T

    val add_transform : object_T -> Transform.transform_T -> unit

    val get_bounding_box : object_T -> AABB.aabb_T

    val check_collision : object_T -> Ray.ray -> Interval.interval_T -> HitRecord.hit

    val scatter_ray : object_T -> Ray.ray -> HitRecord.hit_record -> (Vec3.vec3 * Ray.ray) option

    val to_string : object_T -> string
end
