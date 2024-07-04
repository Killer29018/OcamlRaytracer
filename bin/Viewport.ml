open Vec3
open Camera
open Image

module Viewport = struct
    type viewport_T = {
        width: float;
        height: float;
        depth: float
    }

    let create_null =
        fun () -> {
            width = 0.;
            height = 0.;
            depth = 0.;
        }

    let create width height depth =
        {
            width = width;
            height = height;
            depth = depth
        }

    let create_aspect width depth aspect =
        let height = width /. aspect in
        {
            width = width;
            height = height;
            depth = depth
        }

    let create_vfov width height vfov =
        let depth = height /. (tan (vfov /. 2.)) in
        {
            width = width;
            height = height;
            depth = depth
        }

    let create_vfov_aspect width aspect vfov =
        let height = width /. aspect in
        let depth = height /. (tan (vfov /. 2.)) in
        {
            width = width;
            height = height;
            depth = depth
        }

    let get_components v (i : 'a Image.image) (c : Camera.camera_T) =
        let half_width = v.width /. 2. in
        let half_height = v.height /. 2. in

        let center = Vec3.add (Vec3.scalar c.front v.depth) c.pos in
        let left = Vec3.add (Vec3.scalar (Vec3.negate c.right) half_width) center in

        let top_left = Vec3.add (Vec3.scalar c.up half_height) left in
        let delta_right = Vec3.scalar c.right (v.width /. (float_of_int i.width)) in
        let delta_down = Vec3.scalar (Vec3.negate c.up) (v.height /. (float_of_int i.height)) in

        (top_left, delta_right, delta_down)

    let string_of_viewport v =
        Printf.sprintf "(%.4f:%.4f) | %.4f" v.width v.height v.depth
end
