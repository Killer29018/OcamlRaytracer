open Vec3
open Interval
open Ray

module AABB : sig
    type aabb_T = {
        x : Interval.interval_T;
        y : Interval.interval_T;
        z : Interval.interval_T
    }

    val empty : aabb_T
    val universe : aabb_T

    val create_values : float -> float -> float -> float -> float -> float -> aabb_T

    val create_intervals : Interval.interval_T -> Interval.interval_T -> Interval.interval_T -> aabb_T

    val create_points : Vec3.vec3 -> Vec3.vec3 -> aabb_T

    val create_aabb : aabb_T -> aabb_T -> aabb_T

    val hit : aabb_T -> Ray.ray -> Interval.interval_T -> bool

    val to_string : aabb_T -> string

    val compare_x : aabb_T -> aabb_T -> int
    val compare_y : aabb_T -> aabb_T -> int
    val compare_z : aabb_T -> aabb_T -> int
end
