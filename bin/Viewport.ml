open Vec3
open Camera
open Image

module Viewport = struct
    type viewport_T = {
        viewport_width: float;
        viewport_height: float;
        viewport_depth: float
    }

    let create_null =
        fun () -> {
            viewport_width = 0.;
            viewport_height = 0.;
            viewport_depth = 0.;
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

    let create_vfov width height vfov =
        let depth = (tan (vfov /. 2.)) /. height in
        {
            viewport_width = width;
            viewport_height = height;
            viewport_depth = depth
        }

    let create_vfov_aspect width aspect vfov =
        let height = width /. aspect in
        let depth = (tan (vfov /. 2.)) /. height in
        {
            viewport_width = width;
            viewport_height = height;
            viewport_depth = depth
        }

    let get_components v (i : 'a Image.image) (c : Camera.camera_T) =
        let half_width = v.viewport_width /. 2. in
        let half_height = v.viewport_height /. 2. in

        let center = Vec3.add (Vec3.scalar c.front v.viewport_depth) c.pos in
        let left = Vec3.add (Vec3.scalar (Vec3.negate c.right) half_width) center in

        let top_left = Vec3.add (Vec3.scalar c.up half_height) left in
        let delta_right = Vec3.scalar c.right (v.viewport_width /. (float_of_int i.width)) in
        let delta_down = Vec3.scalar (Vec3.negate c.up) (v.viewport_height /. (float_of_int i.height)) in

        (top_left, delta_right, delta_down)

    let string_of_viewport v =
        Printf.sprintf "(%.4f:%.4f) | %.4f" v.viewport_width v.viewport_height v.viewport_depth
end
