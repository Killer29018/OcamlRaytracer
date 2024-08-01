open Vec3
open HitRecord
open Ray
open AABB
open Interval

module Shape : sig
    type sphere_data = {
        centre: Vec3.vec3;
        radius: float;
    }

    type shape_T = None
                 | Sphere of sphere_data
                (* | Triangle *)

    exception ShapeError of string

    val create_sphere : Vec3.vec3 -> float -> shape_T

    val create_bounding_box : shape_T -> AABB.aabb_T

    val get_normal : shape_T -> Vec3.vec3 -> Vec3.vec3
    val get_normal_and_front_face : shape_T -> Vec3.vec3 -> Vec3.vec3 -> (Vec3.vec3 * bool)

    val check_collision : Ray.ray -> shape_T -> Interval.interval_T -> HitRecord.hit

    val string_of_shape : shape_T -> string
end
