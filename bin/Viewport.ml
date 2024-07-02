open Vec3
open Image

module Viewport = struct
    type viewport = {
        viewport_width: float;
        viewport_height: float;
        viewport_depth: float
    }

    let create width height depth =
        {
            viewport_width = width;
            viewport_height = height;
            viewport_depth = depth
        }

    let create_aspect width depth aspect =
        let height = width /. aspect in
        {
            viewport_width = width;
            viewport_height = height;
            viewport_depth = depth
        }

    let get_components v i =
        let half_width = v.viewport_width /. 2. in
        let half_height = v.viewport_height /. 2. in
        let top_left = Vec3.newV (~-.half_width) (~-.half_height) (v.viewport_depth) in
        let delta_right = Vec3.newV (v.viewport_width /. (float_of_int i.Image.width)) 0. 0. in
        let delta_down = Vec3.newV 0. (v.viewport_height /. (float_of_int i.Image.height)) 0. in
        (top_left, delta_right, delta_down)

    let string_of_viewport v =
        Printf.sprintf "(%.4f:%.4f) | %.4f" v.viewport_width v.viewport_height v.viewport_depth
end
