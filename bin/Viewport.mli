open Vec3
open Image

module Viewport : sig
    type viewport = {
        viewport_width: float;
        viewport_height: float;
        viewport_depth: float
    }

    val create : float -> float -> float -> viewport

    val create_aspect : float -> float -> float -> viewport

    val get_components : viewport -> 'a Image.image -> (Vec3.vec3 * Vec3.vec3 * Vec3.vec3)

    val string_of_viewport : viewport -> string
end
