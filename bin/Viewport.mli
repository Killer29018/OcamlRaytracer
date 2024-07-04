open Vec3
open Image
open Camera

module Viewport : sig
    type viewport_T = {
        width: float;
        height: float;
        depth: float
    }

    val create_null : (unit -> viewport_T)

    val create : float -> float -> float -> viewport_T

    val create_aspect : float -> float -> float -> viewport_T

    val create_vfov : float -> float -> float -> viewport_T
    val create_vfov_aspect_camera : float -> float -> Camera.camera_T -> viewport_T
    val create_vfov_depth_aspect : float -> float -> float -> viewport_T
    val create_vfov_aspect : float -> float -> float -> viewport_T

    val get_components : viewport_T -> 'a Image.image -> Camera.camera_T -> (Vec3.vec3 * Vec3.vec3 * Vec3.vec3)

    val string_of_viewport : viewport_T -> string
end
